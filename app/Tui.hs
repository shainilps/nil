{-# LANGUAGE OverloadedStrings #-}

module Tui (
    Tui,
    Color (..),
    newTui,
    paint,
    readPrompt,
    confirm,
    setBusy,
    spin,
    printText,
    printNotice,
    printToolCall,
    printToolResult,
    printTurnEnd,
) where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Exception (IOException, SomeException, try)
import Control.Monad (forM_, unless, void, when)
import Data.Aeson (Value (..), decodeStrict, encode)
import Data.ByteString.Lazy qualified as BL
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import GHC.Clock (getMonotonicTime)
import System.Console.Haskeline (Settings (..), defaultSettings, getInputLine, handleInterrupt, runInputT, withInterrupt)
import System.Console.Terminfo (getCapability, setupTermFromEnv, termColumns)
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory)
import System.IO (BufferMode (NoBuffering), hIsTerminalDevice, hSetBuffering, hSetEncoding, stdout, utf8)

data Color = Dim | Bold | Red | Green | Yellow | Cyan

data Tui = Tui
    { tuiColor :: Bool
    , tuiTty :: Bool
    , tuiWidth :: Int
    , tuiHistory :: FilePath
    , tuiSpinner :: IORef (Maybe ThreadId)
    , tuiAtLineStart :: IORef Bool
    , tuiBusy :: IORef Bool
    }

spinnerFrames :: [T.Text]
spinnerFrames = ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"]

resultPreviewLines :: Int
resultPreviewLines = 3

showElapsedAfter :: Double
showElapsedAfter = 3

newTui :: FilePath -> IO Tui
newTui historyPath = do
    hSetBuffering stdout NoBuffering
    hSetEncoding stdout utf8
    tty <- hIsTerminalDevice stdout
    noColor <- lookupEnv "NO_COLOR"
    width <- terminalWidth
    void (try @IOException (createDirectoryIfMissing True (takeDirectory historyPath)))
    Tui (tty && isNothing noColor) tty width historyPath
        <$> newIORef Nothing
        <*> newIORef True
        <*> newIORef False

terminalWidth :: IO Int
terminalWidth = do
    r <- try @SomeException setupTermFromEnv
    pure $ fromMaybe 80 $ either (const Nothing) (`getCapability` termColumns) r

paint :: Tui -> Color -> T.Text -> T.Text
paint tui c s
    | tuiColor tui = "\ESC[" <> code c <> "m" <> s <> "\ESC[0m"
    | otherwise = s
  where
    code Dim = "2"
    code Bold = "1"
    code Red = "31"
    code Green = "32"
    code Yellow = "33"
    code Cyan = "36"

readPrompt :: Tui -> IO (Maybe T.Text)
readPrompt tui = do
    let settings = (defaultSettings :: Settings IO){historyFile = Just (tuiHistory tui)}
        prompt = paint tui Bold (paint tui Cyan "❯ ")
    line <- runInputT settings $ handleInterrupt (pure Nothing) $ withInterrupt $ getInputLine (T.unpack prompt)
    case T.strip . T.pack <$> line of
        Nothing -> pure Nothing
        Just "" -> readPrompt tui
        Just t -> pure (Just t)

confirm :: Tui -> T.Text -> IO T.Text
confirm tui question = do
    stopSpinner tui
    TIO.putStr (paint tui Yellow question)
    answer <- try @IOException TIO.getLine
    writeIORef (tuiAtLineStart tui) True
    pure $ either (const "n") (T.toLower . T.strip) answer

-- every visible write goes through here so the spinner never mixes in
write :: Tui -> T.Text -> IO ()
write tui s = unless (T.null s) $ do
    stopSpinner tui
    TIO.putStr s
    writeIORef (tuiAtLineStart tui) ("\n" `T.isSuffixOf` s)

newline :: Tui -> IO ()
newline tui = do
    atStart <- readIORef (tuiAtLineStart tui)
    unless atStart (write tui "\n")

printText :: Tui -> T.Text -> IO ()
printText = write

printNotice :: Tui -> Color -> T.Text -> IO ()
printNotice tui tint t = do
    newline tui
    write tui (paint tui tint t <> "\n")

printToolCall :: Tui -> T.Text -> T.Text -> IO ()
printToolCall tui name rawArgs = do
    newline tui
    let args = fromMaybe Null (decodeStrict (TE.encodeUtf8 rawArgs))
        summary = truncateTo (tuiWidth tui - T.length name - 4) (compactJson args)
    write tui (paint tui Cyan "●" <> " " <> paint tui Bold name <> " " <> summary <> "\n")

printToolResult :: Tui -> T.Text -> IO ()
printToolResult tui result = do
    newline tui
    let failed = any (`T.isPrefixOf` result) ["error:", "[exit", "Exit code: ExitFailure"]
        tint = if failed then Red else Dim
        ls = T.lines (T.stripEnd result)
        empty = T.null (T.strip result)
        shown = if empty then ["(no output)"] else take resultPreviewLines ls
        w = tuiWidth tui - 4
    forM_ (zip [0 :: Int ..] shown) $ \(i, l) ->
        write tui ((if i == 0 then "  ⎿ " else "    ") <> paint tui tint (truncateTo w l) <> "\n")
    let hidden = length ls - length shown
    when (not empty && hidden > 0) $
        write tui ("    " <> paint tui Dim ("… " <> T.pack (show hidden) <> " more lines") <> "\n")
    busy <- readIORef (tuiBusy tui)
    when busy (startSpinner tui "thinking")

printTurnEnd :: Tui -> Maybe T.Text -> IO ()
printTurnEnd tui stats = do
    newline tui
    forM_ stats $ \s -> write tui (paint tui Dim ("✓ " <> s) <> "\n")
    write tui "\n"

setBusy :: Tui -> Bool -> IO ()
setBusy tui b = do
    writeIORef (tuiBusy tui) b
    if b then startSpinner tui "thinking" else stopSpinner tui

spin :: Tui -> T.Text -> IO ()
spin tui label = stopSpinner tui >> startSpinner tui label

startSpinner :: Tui -> T.Text -> IO ()
startSpinner tui label = do
    running <- readIORef (tuiSpinner tui)
    when (tuiTty tui && isNothing running) $ do
        newline tui
        started <- getMonotonicTime
        let draw i = do
                now <- getMonotonicTime
                let secs = now - started
                    elapsed
                        | secs >= showElapsedAfter = " " <> T.pack (show (floor secs :: Int)) <> "s"
                        | otherwise = ""
                    frame = spinnerFrames !! (i `mod` length spinnerFrames)
                TIO.putStr ("\r" <> paint tui Cyan frame <> " " <> paint tui Dim (label <> "…" <> elapsed))
                threadDelay 80000
                draw (i + 1)
        tid <- forkIO (draw (0 :: Int))
        writeIORef (tuiSpinner tui) (Just tid)

stopSpinner :: Tui -> IO ()
stopSpinner tui = do
    running <- readIORef (tuiSpinner tui)
    forM_ running $ \tid -> do
        killThread tid
        writeIORef (tuiSpinner tui) Nothing
        TIO.putStr "\r\ESC[K"

compactJson :: Value -> T.Text
compactJson = TE.decodeUtf8 . BL.toStrict . encode

truncateTo :: Int -> T.Text -> T.Text
truncateTo maxW s
    | T.length s > m = T.take (m - 1) s <> "…"
    | otherwise = s
  where
    m = max 10 maxW
