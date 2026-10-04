{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Agent
import Control.Exception (IOException, SomeException, displayException, try)
import Control.Monad (forM_, mfilter, when)
import Data.Aeson (Value (..), decodeStrict, encode)
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.Char (isSpace)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (find, intercalate, isPrefixOf, isSuffixOf, maximumBy)
import Data.Either (fromRight)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import GHC.Clock (getMonotonicTime)
import Llm
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, getModificationTime, listDirectory)
import System.Environment (getArgs, lookupEnv)
import System.Exit (die)
import System.FilePath (takeBaseName, takeDirectory, (</>))
import System.IO (IOMode (ReadMode), withBinaryFile)
import Text.Printf (printf)
import Tool (tools)
import Tui

sessionDir :: FilePath
sessionDir = ".nil"

basePrompt :: FilePath -> T.Text
basePrompt cwd =
    T.intercalate
        "\n"
        [ "You are nil, a coding agent. Working directory: " <> T.pack cwd
        , "Use tools to inspect files and run commands; never guess file contents."
        , "- Read a file before editing it. Prefer edit_file over write_file for existing files."
        , "- After changes, verify (build, test, or re-read) when practical."
        , "- Be brief. Say what you changed; don't paste whole files back."
        ]

data Resume = NoResume | ResumeLatest | ResumeId String

data Args = Args
    { argAuto :: Bool
    , argResume :: Resume
    }

parseArgs :: [String] -> Either String Args
parseArgs = go (Args False NoResume)
  where
    go a [] = Right a
    go a ("--auto" : rest) = go a{argAuto = True} rest
    go a ("--resume" : next : rest)
        | not ("-" `isPrefixOf` next) = go a{argResume = ResumeId next} rest
    go a ("--resume" : rest) = go a{argResume = ResumeLatest} rest
    go _ (x : _) = Left ("unknown argument: " <> x <> "\nusage: nil [--auto] [--resume [id]]")

sessionFiles :: IO [FilePath]
sessionFiles = do
    r <- try @IOException (listDirectory sessionDir)
    pure [sessionDir </> n | n <- fromRight [] r, ".jsonl" `isSuffixOf` n]

resolveSession :: Resume -> IO FilePath
resolveSession NoResume = do
    bytes <- withBinaryFile "/dev/urandom" ReadMode (`BS.hGet` 4)
    pure (sessionDir </> concatMap (printf "%02x") (BS.unpack bytes) <> ".jsonl")
resolveSession (ResumeId sid) = do
    files <- sessionFiles
    case filter ((sid `isPrefixOf`) . takeBaseName) files of
        [one] -> pure one
        [] -> die ("session " <> sid <> " not found in " <> sessionDir <> " (sessions are saved after the first message)")
        ms -> die ("session id \"" <> sid <> "\" is ambiguous: " <> intercalate ", " (map takeBaseName ms))
resolveSession ResumeLatest = do
    files <- sessionFiles
    when (null files) $ die ("no sessions to resume in " <> sessionDir)
    times <- mapM getModificationTime files
    pure (snd (maximumBy (comparing fst) (zip times files)))

loadSession :: FilePath -> IO [Message]
loadSession file = do
    r <- try @IOException (BS.readFile file)
    pure $ case r of
        Left _ -> []
        Right bytes -> mapMaybe decodeStrict (filter (not . BS.null) (BC.lines bytes))

persistSession :: IORef Int -> FilePath -> [Message] -> IO ()
persistSession countRef file msgs = do
    createDirectoryIfMissing True (takeDirectory file)
    count <- readIORef countRef
    if length msgs < count
        then BL.writeFile file (jsonl msgs)
        else BL.appendFile file (jsonl (drop count msgs))
    writeIORef countRef (length msgs)
  where
    jsonl = BL.concat . map (\m -> encode m <> "\n")

loadAgentsMd :: IO T.Text
loadAgentsMd = do
    r <- try @IOException (TIO.readFile "AGENTS.md")
    pure $ either (const "") ("\n\n# Project instructions (AGENTS.md)\n" <>) r

data App = App
    { tui :: Tui
    , modelRef :: IORef Model
    , autoRef :: IORef Bool
    , allowRef :: IORef (Set T.Text)
    , modelsRef :: IORef (Maybe [ModelInfo])
    , persistedRef :: IORef Int
    , sessionFile :: FilePath
    }

say :: App -> T.Text -> IO ()
say app = printText app.tui

approve :: App -> Approve
approve app name rawArgs = do
    auto <- readIORef app.autoRef
    allowed <- Set.member name <$> readIORef app.allowRef
    if auto || allowed
        then pure True
        else do
            say app (preview app.tui name rawArgs)
            answer <- confirm app.tui "  allow? [y/n/a] "
            when (answer == "a") $ modifyIORef' app.allowRef (Set.insert name)
            pure (answer `elem` ["y", "a"])

preview :: Tui -> T.Text -> T.Text -> T.Text
preview t name rawArgs
    | name /= "edit_file" = ""
    | otherwise = block "-" Red (get "old_content") <> "\n" <> block "+" Green (get "new_content") <> "\n"
  where
    args = decodeStrict (TE.encodeUtf8 rawArgs) :: Maybe Value
    get k = case args of
        Just (Object o) | Just (String s) <- KM.lookup k o -> s
        _ -> ""
    block sign c txt = T.intercalate "\n" [paint t c ("  " <> sign <> " " <> l) | l <- T.splitOn "\n" txt]

getModels :: App -> IO (Either T.Text [ModelInfo])
getModels app = do
    cached <- readIORef app.modelsRef
    case cached of
        Just ms -> pure (Right ms)
        Nothing -> do
            r <- readIORef app.modelRef >>= listModels
            forM_ r (writeIORef app.modelsRef . Just)
            pure r

runCommand :: App -> T.Text -> T.Text -> IO ()
runCommand app name arg = case name of
    "help" ->
        say app . T.unlines $
            [ "/models [filter]  list chat models (tool support shown when known)"
            , "/model [id]       show or switch the current model"
            , "/mode [auto|manual]  show or switch tool approval mode"
            , "/help             this list"
            ]
    "mode" -> case arg of
        "auto" -> writeIORef app.autoRef True >> showMode
        "manual" -> writeIORef app.autoRef False >> writeIORef app.allowRef Set.empty >> showMode
        "" -> showMode
        _ -> say app "usage: /mode [auto|manual]\n"
    "models" -> do
        r <- getModels app
        case r of
            Left e -> printNotice app.tui Red ("[error] " <> e)
            Right ms -> do
                current <- (.model) <$> readIORef app.modelRef
                let list = filter ((arg `T.isInfixOf`) . modelId) ms
                    usable = filter ((/= Just False) . modelTools) list
                    hidden = length list - length usable
                forM_ usable $ \mi ->
                    say app ((if modelId mi == current then "*" else " ") <> " " <> modelId mi <> "\n")
                say app $
                    tshow (length usable)
                        <> (if length usable == 1 then " model" else " models")
                        <> (if hidden > 0 then " (" <> tshow hidden <> " without tool support hidden)" else "")
                        <> "\n"
    "model"
        | T.null arg -> do
            m <- readIORef app.modelRef
            say app ("current model: " <> m.model <> "\n")
        | otherwise -> do
            r <- getModels app
            let info = either (const Nothing) (find ((== arg) . modelId)) r
            case (r, info) of
                (Right _, Nothing) -> say app ("unknown model \"" <> arg <> "\", see /models " <> arg <> "\n")
                _ -> do
                    case r of
                        Left e -> say app ("couldn't verify model (" <> e <> ")\n")
                        Right _ -> pure ()
                    when ((info >>= modelTools) == Just False) $
                        say app "warning: this model doesn't support tools\n"
                    modifyIORef' app.modelRef (\m -> m{model = arg})
                    say app ("switched to " <> arg <> "\n")
    _ -> say app ("unknown command /" <> name <> ", try /help\n")
  where
    showMode = do
        auto <- readIORef app.autoRef
        say app ("mode: " <> (if auto then "auto" else "manual") <> "\n")

turnStats :: Double -> TokenUsage -> T.Text
turnStats secs u
    | usageIn u == 0 && usageOut u == 0 = time
    | otherwise = time <> " · ↑" <> k (usageIn u) <> " ↓" <> k (usageOut u) <> " tokens"
  where
    time = T.pack (printf "%.1fs" secs)
    k n
        | n >= 1000 = T.pack (printf "%.1fk" (fromIntegral n / 1000 :: Double))
        | otherwise = tshow n

onEvent :: App -> Double -> AgentEvent -> IO ()
onEvent app started ev = case ev of
    AssistantText d -> say app d
    ToolCalled _ n a -> printToolCall app.tui n a
    ToolStarted{} -> spin app.tui "running"
    ToolFinished _ _ r -> printToolResult app.tui r
    TurnEnded stop u -> do
        when (stop == TurnMaxTokens) $ printNotice app.tui Yellow "[output truncated by max_tokens]"
        when (stop == TurnError) $ printNotice app.tui Red "[error occurred]"
        now <- getMonotonicTime
        printTurnEnd app.tui (Just (turnStats (now - started) u))

runTurn :: App -> Double -> Context -> T.Text -> IO Context
runTurn app started ctx text = do
    let ctx1 = ctx{messages = ctx.messages ++ [Message User (TextContent text)]}
    m <- readIORef app.modelRef
    r <- try @SomeException $ do
        ctx2 <- runAgent m ctx1 tools (approve app) (onEvent app started)
        persistSession app.persistedRef app.sessionFile ctx2.messages
        pure ctx2
    case r of
        Right ctx2 -> pure ctx2
        Left e -> do
            printNotice app.tui Red ("[error] " <> T.pack (displayException e))
            pure ctx1

repl :: App -> Context -> IO ()
repl app ctx = do
    line <- readPrompt app.tui
    case line of
        Nothing -> pure ()
        Just text -> do
            started <- getMonotonicTime
            setBusy app.tui True
            ctx' <- case T.stripPrefix "/" text of
                Just cmd -> do
                    let (name, rest) = T.break isSpace cmd
                    runCommand app name (T.strip rest)
                    pure ctx
                Nothing -> runTurn app started ctx text
            setBusy app.tui False
            repl app ctx'

env :: String -> IO (Maybe T.Text)
env k = fmap T.pack . mfilter (not . null) <$> lookupEnv k

main :: IO ()
main = do
    args <- getArgs >>= either die pure . parseArgs
    key <- env "NIL_API_KEY" >>= maybe (die "NIL_API_KEY is not set") pure
    modelName <- fromMaybe "gpt-5.4-nano" <$> env "NIL_MODEL"
    base <- env "NIL_BASE_URL"

    file <- resolveSession args.argResume
    msgs <- loadSession file
    cwd <- getCurrentDirectory
    agentsMd <- loadAgentsMd
    t <- newTui (sessionDir </> "history")
    app <-
        App t
            <$> newIORef (Model key modelName base (Just 4096))
            <*> newIORef args.argAuto
            <*> newIORef Set.empty
            <*> newIORef Nothing
            <*> newIORef (length msgs)
            <*> pure file

    let sid = T.pack (takeBaseName file)
    printNotice t Dim $
        T.intercalate
            " · "
            [ if null msgs then "session " <> sid else "resumed session " <> sid <> " (" <> tshow (length msgs) <> " messages)"
            , modelName
            , if args.argAuto then "auto" else "manual"
            , "/help for commands"
            ]
            <> "\n"

    repl app (Context (Just (basePrompt cwd <> agentsMd)) msgs)
