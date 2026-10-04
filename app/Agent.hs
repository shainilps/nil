{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Agent (AgentEvent (..), TokenUsage (..), TurnStop (..), Approve, runAgent) where

import Control.Exception (AsyncException (UserInterrupt), IOException, displayException, throwIO, try)
import Control.Monad (forM_)
import Data.Aeson (Value, decodeStrict, object)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Llm
import Tool (Tool (..), ToolResult (..))

type Call = (T.Text, T.Text, T.Text)

type Approve = T.Text -> T.Text -> IO Bool

data TokenUsage = TokenUsage
    { usageIn :: Int
    , usageOut :: Int
    }
    deriving (Show, Eq)

data TurnStop
    = TurnDone
    | TurnMaxTokens
    | TurnAborted
    | TurnError
    deriving (Show, Eq)

data AgentEvent
    = AssistantText T.Text
    | ToolCalled T.Text T.Text T.Text
    | ToolStarted T.Text T.Text T.Text
    | ToolFinished T.Text T.Text T.Text
    | TurnEnded TurnStop TokenUsage
    deriving (Show, Eq)

compactThreshold, keepRecent :: Int
compactThreshold = 50
keepRecent = 20

abortedMsg :: T.Text
abortedMsg = "error: aborted"

report :: (AgentEvent -> IO ()) -> Call -> T.Text -> IO ()
report emit (i, n, a) result = emit (ToolCalled i n a) >> emit (ToolFinished i n result)

push :: Message -> Context -> Context
push msg c = c{messages = c.messages ++ [msg]}

argsValue :: T.Text -> Value
argsValue a = fromMaybe (object []) (decodeStrict (TE.encodeUtf8 a))

renderMessage :: Message -> T.Text
renderMessage (Message r c) = roleText r <> ": " <> body
  where
    body = case c of
        TextContent t -> t
        BlocksContent bs -> T.unwords (map renderBlock bs)
    renderBlock (TextBlock t) = t
    renderBlock (ToolUseBlock _ n a) = "[tool_use " <> n <> " " <> a <> "]"
    renderBlock (ToolResultBlock _ t) = "[tool_result " <> t <> "]"

compactContext :: Model -> Context -> IO Context
compactContext m ctx
    | length msgs < compactThreshold = pure ctx
    | otherwise = do
        ref <- newIORef (Just "")
        stream m summaryCtx [] $ \case
            TextDelta d -> modifyIORef' ref (fmap (<> d))
            StreamError _ -> writeIORef ref Nothing
            Done Aborted -> writeIORef ref Nothing
            _ -> pure ()
        result <- readIORef ref
        pure $ case result of
            Just s
                | not (T.null s) ->
                    ctx{messages = Message User (TextContent ("[context summary]\n" <> s)) : recent}
            _ -> ctx
  where
    msgs = ctx.messages
    (old, recent) = splitAt (length msgs - keepRecent) msgs
    summaryCtx =
        Context
            (Just "summarise this context")
            [Message User (TextContent (T.intercalate "\n" (map renderMessage old)))]

data Turn = Turn
    { turnText :: T.Text
    , turnCalls :: [Call]
    , turnStop :: StopReason
    , turnError :: Maybe T.Text
    }

streamTurn :: Model -> Context -> [ToolDef] -> IORef TokenUsage -> (AgentEvent -> IO ()) -> IO Turn
streamTurn m ctx defs usageRef emit = do
    ref <- newIORef (Turn "" [] EndTurn Nothing)
    stream m ctx defs $ \case
        TextDelta d -> do
            modifyIORef' ref (\t -> t{turnText = turnText t <> d})
            emit (AssistantText d)
        ToolCall i n a -> modifyIORef' ref (\t -> t{turnCalls = turnCalls t ++ [(i, n, a)]})
        Usage i o -> modifyIORef' usageRef (\u -> TokenUsage (usageIn u + i) (usageOut u + o))
        Done r -> modifyIORef' ref (\t -> t{turnStop = r})
        StreamError e -> modifyIORef' ref (\t -> t{turnError = Just e})
    readIORef ref

runOne :: Map T.Text Tool -> Approve -> (AgentEvent -> IO ()) -> Call -> IO T.Text
runOne toolMap approve emit (i, n, a) =
    case Map.lookup n toolMap of
        Nothing -> pure ("error: tool \"" <> n <> "\" not found")
        Just tool -> do
            allowed <- if tool.needApproval then approve n a else pure True
            if not allowed
                then pure "error: user denied this tool call"
                else do
                    emit (ToolStarted i n a)
                    r <- try @IOException (tool.execute (argsValue a))
                    pure $ case r of
                        Left e -> "error: " <> T.pack (displayException e)
                        Right (ToolSuccess t) -> t
                        Right (ToolError t) -> "error: " <> t

-- every tool_call needs a tool result, or the API rejects the next request
runToolCalls :: Map T.Text Tool -> Approve -> (AgentEvent -> IO ()) -> [Call] -> IO ([(T.Text, T.Text)], Bool)
runToolCalls toolMap approve emit = go []
  where
    go acc [] = pure (reverse acc, False)
    go acc (call@(i, n, a) : rest) = do
        emit (ToolCalled i n a)
        r <- try @AsyncException (runOne toolMap approve emit call)
        case r of
            Right res -> do
                emit (ToolFinished i n res)
                go ((i, res) : acc) rest
            Left UserInterrupt -> do
                emit (ToolFinished i n abortedMsg)
                forM_ rest $ \c -> report emit c abortedMsg
                pure (reverse acc ++ [(j, abortedMsg) | (j, _, _) <- call : rest], True)
            Left e -> throwIO e

runAgent :: Model -> Context -> [Tool] -> Approve -> (AgentEvent -> IO ()) -> IO Context
runAgent m ctx0 tools approve emit = do
    usageRef <- newIORef (TokenUsage 0 0)
    let toolMap = Map.fromList [(t.name, t) | t <- tools]
        defs = [ToolDef t.name t.description t.parameters | t <- tools]

        finish ctx stop = do
            u <- readIORef usageRef
            emit (TurnEnded stop u)
            pure ctx

        loop ctxIn = do
            ctx <- compactContext m ctxIn
            turn <- streamTurn m ctx defs usageRef emit
            let calls = turnCalls turn
                assistant = buildAssistantMessage (turnText turn)
                withCalls = push (assistant calls) ctx
            case (turnError turn, turnStop turn) of
                (Just e, _) -> do
                    emit (AssistantText ("\n[error] " <> e))
                    finish (push (assistant []) ctx) TurnError
                (_, Aborted) ->
                    finish (push (assistant []) ctx) TurnAborted
                (_, MaxTokens)
                    | not (null calls) -> do
                        let results =
                                [ (i, "error: output truncated by max_tokens, tool \"" <> n <> "\" args may be incomplete.")
                                | (i, n, _) <- calls
                                ]
                        forM_ (zip calls results) $ \(c, (_, r)) -> report emit c r
                        loop (push (buildToolResultMessage results) withCalls)
                (_, stop)
                    | null calls ->
                        finish withCalls (if stop == MaxTokens then TurnMaxTokens else TurnDone)
                _ -> do
                    (results, aborted) <- runToolCalls toolMap approve emit calls
                    let ctx' = push (buildToolResultMessage results) withCalls
                    if aborted then finish ctx' TurnAborted else loop ctx'

    loop ctx0
