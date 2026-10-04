{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Llm where

import Control.Exception (AsyncException (UserInterrupt), SomeException, displayException, fromException, try)
import Control.Monad (foldM, forM_, mfilter)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (Null, String), decodeStrict, eitherDecode, encode, object, withObject, withText, (.!=), (.:), (.:?), (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.List (foldl', sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (lenientDecode)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (getGlobalManager)
import Network.HTTP.Types.Status (statusCode, statusIsSuccessful)

data Model = Model
    { apiKey :: T.Text
    , model :: T.Text
    , baseUrl :: Maybe T.Text
    , maxTokens :: Maybe Int
    }

data Role
    = User
    | Assistant
    deriving (Show, Eq)

data ContentBlock
    = TextBlock
        { text :: T.Text
        }
    | ToolUseBlock
        { toolCallId :: T.Text
        , toolName :: T.Text
        , toolInput :: T.Text
        }
    | ToolResultBlock
        { toolUseId :: T.Text
        , content :: T.Text
        }
    deriving (Show, Eq)

data MessageContent
    = TextContent T.Text
    | BlocksContent [ContentBlock]
    deriving (Show, Eq)

data Message = Message
    { role :: Role
    , content :: MessageContent
    }
    deriving (Show, Eq)

data Context = Context
    { systemPrompt :: Maybe T.Text
    , messages :: [Message]
    }

data StopReason
    = EndTurn
    | ToolUse
    | MaxTokens
    | Aborted
    deriving (Show, Eq)

data StreamEvent
    = TextDelta
        { delta :: T.Text
        }
    | ToolCall
        { toolCallId :: T.Text
        , toolCallName :: T.Text
        , toolCallArgs :: T.Text
        }
    | Done
        { stopReason :: StopReason
        }
    | Usage
        { inputTokens :: Int
        , outputTokens :: Int
        }
    | StreamError
        { errorMessage :: T.Text
        }
    deriving (Show, Eq)

data ToolDef = ToolDef
    { name :: T.Text
    , description :: T.Text
    , parameters :: Value
    }

roleText :: Role -> T.Text
roleText User = "user"
roleText Assistant = "assistant"

-- sessions store tool_use input as a JSON object, not a string

instance ToJSON Role where
    toJSON = String . roleText

instance FromJSON Role where
    parseJSON = withText "role" $ \t -> case t of
        "user" -> pure User
        "assistant" -> pure Assistant
        _ -> fail ("unknown role " <> T.unpack t)

instance ToJSON ContentBlock where
    toJSON (TextBlock t) = object ["type" .= String "text", "text" .= t]
    toJSON (ToolUseBlock i n a) =
        object
            [ "type" .= String "tool_use"
            , "id" .= i
            , "name" .= n
            , "input" .= fromMaybe (object []) (decodeStrict (TE.encodeUtf8 a) :: Maybe Value)
            ]
    toJSON (ToolResultBlock i c) =
        object ["type" .= String "tool_result", "tool_use_id" .= i, "content" .= c]

instance FromJSON ContentBlock where
    parseJSON = withObject "block" $ \o -> do
        ty <- o .: "type"
        case ty :: T.Text of
            "text" -> TextBlock <$> o .: "text"
            "tool_use" -> ToolUseBlock <$> o .: "id" <*> o .: "name" <*> (inputText <$> o .:? "input" .!= object [])
            "tool_result" -> ToolResultBlock <$> o .: "tool_use_id" <*> o .: "content"
            _ -> fail ("unknown block type " <> T.unpack ty)
      where
        inputText (String s) = s
        inputText v = decodeLenient (BL.toStrict (encode v))

instance ToJSON MessageContent where
    toJSON (TextContent t) = String t
    toJSON (BlocksContent bs) = toJSON bs

instance FromJSON MessageContent where
    parseJSON (String t) = pure (TextContent t)
    parseJSON v = BlocksContent <$> parseJSON v

instance ToJSON Message where
    toJSON (Message r c) = object ["role" .= r, "content" .= c]

instance FromJSON Message where
    parseJSON = withObject "message" $ \o -> Message <$> o .: "role" <*> o .: "content"

contextToOpenAIMessages :: Context -> [Value]
contextToOpenAIMessages (Context sys msgs) =
    [ object ["role" .= String "system", "content" .= s]
    | Just s <- [sys]
    , not (T.null s)
    ]
        ++ concatMap messageToJSON msgs

messageToJSON :: Message -> [Value]
messageToJSON (Message r (TextContent t)) =
    [object ["role" .= roleText r, "content" .= t]]
messageToJSON (Message Assistant (BlocksContent bs)) =
    [ object $
        [ "role" .= String "assistant"
        , "content" .= contentValue
        ]
            ++ ["tool_calls" .= calls | not (null calls)]
    ]
  where
    txt = T.concat [t | TextBlock t <- bs]
    calls =
        [ object
            [ "id" .= i
            , "type" .= String "function"
            , "function" .= object ["name" .= n, "arguments" .= args]
            ]
        | ToolUseBlock i n args <- bs
        ]
    contentValue
        | not (T.null txt) = toJSON txt
        | null calls = toJSON ("" :: T.Text)
        | otherwise = Null
messageToJSON (Message User (BlocksContent bs)) = concatMap one bs
  where
    one (ToolResultBlock i c) =
        [object ["role" .= String "tool", "tool_call_id" .= i, "content" .= c]]
    one (TextBlock t) = [object ["role" .= String "user", "content" .= t]]
    one ToolUseBlock{} = []

data RawChunk = RawChunk [RawChoice] (Maybe RawUsage)
data RawChoice = RawChoice (Maybe RawDelta) (Maybe T.Text)
data RawDelta = RawDelta (Maybe T.Text) [RawToolDelta]
data RawToolDelta = RawToolDelta (Maybe Int) (Maybe T.Text) (Maybe RawFn)
data RawFn = RawFn (Maybe T.Text) (Maybe T.Text)
data RawUsage = RawUsage Int Int

instance FromJSON RawChunk where
    parseJSON = withObject "chunk" $ \o ->
        RawChunk <$> o .:? "choices" .!= [] <*> o .:? "usage"

instance FromJSON RawChoice where
    parseJSON = withObject "choice" $ \o ->
        RawChoice <$> o .:? "delta" <*> o .:? "finish_reason"

instance FromJSON RawDelta where
    parseJSON = withObject "delta" $ \o ->
        RawDelta <$> o .:? "content" <*> o .:? "tool_calls" .!= []

instance FromJSON RawToolDelta where
    parseJSON = withObject "tool_call" $ \o ->
        RawToolDelta <$> o .:? "index" <*> o .:? "id" <*> o .:? "function"

instance FromJSON RawFn where
    parseJSON = withObject "function" $ \o ->
        RawFn <$> o .:? "name" <*> o .:? "arguments"

instance FromJSON RawUsage where
    parseJSON = withObject "usage" $ \o ->
        RawUsage <$> o .:? "prompt_tokens" .!= 0 <*> o .:? "completion_tokens" .!= 0

data ToolBuf = ToolBuf
    { tbId :: T.Text
    , tbName :: T.Text
    , tbArgs :: [T.Text]
    }

data St = St
    { stBuf :: BS.ByteString
    , stTools :: Map Int ToolBuf
    , stStop :: StopReason
    }

tshow :: (Show a) => a -> T.Text
tshow = T.pack . show

nonEmpty :: Maybe T.Text -> Maybe T.Text
nonEmpty = mfilter (not . T.null)

addToolDelta :: Map Int ToolBuf -> RawToolDelta -> Map Int ToolBuf
addToolDelta m (RawToolDelta mIdx mId mFn) =
    Map.insert i updated m
  where
    i = fromMaybe 0 mIdx
    existing = Map.findWithDefault (ToolBuf ("call_" <> tshow i) "" []) i m
    (mName, mArgs) = case mFn of
        Nothing -> (Nothing, Nothing)
        Just (RawFn n a) -> (nonEmpty n, nonEmpty a)
    updated =
        existing
            { tbId = fromMaybe (tbId existing) (nonEmpty mId)
            , tbName = fromMaybe (tbName existing) mName
            , tbArgs = maybe id (:) mArgs (tbArgs existing)
            }

finishToStop :: T.Text -> Maybe StopReason
finishToStop "tool_calls" = Just ToolUse
finishToStop "length" = Just MaxTokens
finishToStop _ = Nothing

normalizeArgs :: T.Text -> T.Text
normalizeArgs t = case decodeStrict (TE.encodeUtf8 t) :: Maybe Value of
    Just _ | not (T.null t) -> t
    _ -> "{}"

handleChunk :: (StreamEvent -> IO ()) -> St -> RawChunk -> IO St
handleChunk emit st (RawChunk choices usage) = do
    forM_ (nonEmpty (dl >>= \(RawDelta c _) -> c)) $ \t -> emit (TextDelta t)
    forM_ usage $ \(RawUsage i o) -> emit (Usage i o)
    pure
        st
            { stTools = foldl' addToolDelta (stTools st) (maybe [] (\(RawDelta _ tcs) -> tcs) dl)
            , stStop = fromMaybe (stStop st) (choice >>= \(RawChoice _ f) -> f >>= finishToStop)
            }
  where
    choice = listToMaybe choices
    dl = choice >>= \(RawChoice d _) -> d

handleLine :: (StreamEvent -> IO ()) -> St -> BS.ByteString -> IO St
handleLine emit st raw =
    case BS.stripPrefix "data:" (BC.strip raw) of
        Nothing -> pure st
        Just rest
            | payload == "[DONE]" -> pure st
            | otherwise -> maybe (pure st) (handleChunk emit st) (decodeStrict payload)
          where
            payload = BC.strip rest

readLoop :: BodyReader -> (StreamEvent -> IO ()) -> St -> IO St
readLoop br emit st = do
    chunk <- brRead br
    if BS.null chunk
        then pure st
        else do
            let pieces = BC.split '\n' (stBuf st <> chunk)
                (complete, rest) = case reverse pieces of
                    (r : revDone) -> (reverse revDone, r)
                    [] -> ([], BS.empty)
            st' <- foldM (handleLine emit) st{stBuf = rest} complete
            readLoop br emit st'

mkRequest :: Model -> String -> IO Request
mkRequest m path' = do
    let base = T.dropWhileEnd (== '/') (fromMaybe "https://api.openai.com/v1" (baseUrl m))
    req <- parseRequest (T.unpack base <> path')
    pure
        req
            { requestHeaders =
                [ ("Content-Type", "application/json")
                , ("Authorization", "Bearer " <> TE.encodeUtf8 (apiKey m))
                ]
            , responseTimeout = responseTimeoutNone
            }

chatBody :: Model -> Context -> [ToolDef] -> Value
chatBody m ctx tools =
    object $
        [ "model" .= model m
        , "stream" .= True
        , "stream_options" .= object ["include_usage" .= True]
        , "messages" .= contextToOpenAIMessages ctx
        ]
            ++ ["max_tokens" .= n | Just n <- [maxTokens m]]
            ++ [ "tools"
                    .= [ object
                            [ "type" .= String "function"
                            , "function" .= object ["name" .= n, "description" .= d, "parameters" .= p]
                            ]
                       | ToolDef n d p <- tools
                       ]
               | not (null tools)
               ]

decodeLenient :: BS.ByteString -> T.Text
decodeLenient = TE.decodeUtf8With lenientDecode

stream :: Model -> Context -> [ToolDef] -> (StreamEvent -> IO ()) -> IO ()
stream m ctx tools emit = do
    result <- try @SomeException (streamRequest m ctx tools emit)
    case result of
        Right () -> pure ()
        Left e
            | fromException e == Just UserInterrupt -> emit (Done Aborted)
            | otherwise -> emit (StreamError (T.pack (displayException e)))

streamRequest :: Model -> Context -> [ToolDef] -> (StreamEvent -> IO ()) -> IO ()
streamRequest m ctx tools emit = do
    manager <- getGlobalManager
    req0 <- mkRequest m "/chat/completions"
    let req = req0{method = "POST", requestBody = RequestBodyLBS (encode (chatBody m ctx tools))}
    withResponse req manager $ \resp -> do
        let status = responseStatus resp
        if statusIsSuccessful status
            then do
                final <- readLoop (responseBody resp) emit (St BS.empty Map.empty EndTurn)
                forM_ (Map.toAscList (stTools final)) $ \(_, tb) ->
                    emit (ToolCall (tbId tb) (tbName tb) (normalizeArgs (T.concat (reverse (tbArgs tb)))))
                emit (Done (stStop final))
            else do
                body <- BS.concat <$> brConsume (responseBody resp)
                emit (StreamError ("API " <> tshow (statusCode status) <> ": " <> decodeLenient body))

buildAssistantMessage :: T.Text -> [(T.Text, T.Text, T.Text)] -> Message
buildAssistantMessage txt calls =
    Message Assistant . BlocksContent $
        [TextBlock txt | not (T.null txt)]
            ++ [ToolUseBlock i n a | (i, n, a) <- calls]

buildToolResultMessage :: [(T.Text, T.Text)] -> Message
buildToolResultMessage rs =
    Message User (BlocksContent [ToolResultBlock i c | (i, c) <- rs])

data ModelInfo = ModelInfo
    { modelId :: T.Text
    , modelTools :: Maybe Bool
    }
    deriving (Show, Eq)

data RawModel = RawModel T.Text (Maybe [T.Text])

instance FromJSON RawModel where
    parseJSON = withObject "model" $ \o ->
        RawModel <$> o .: "id" <*> o .:? "supported_parameters"

newtype RawModels = RawModels [RawModel]

instance FromJSON RawModels where
    parseJSON = withObject "models" $ \o -> RawModels <$> o .: "data"

nonChatWords :: [T.Text]
nonChatWords =
    ["tts", "whisper", "audio", "realtime", "transcribe", "image", "sora", "embedding", "moderation", "davinci", "babbage", "search", "safety"]

listModels :: Model -> IO (Either T.Text [ModelInfo])
listModels m = do
    r <- try @SomeException $ do
        manager <- getGlobalManager
        req <- mkRequest m "/models"
        httpLbs req manager
    pure $ case r of
        Left e -> Left (T.pack (displayException e))
        Right resp
            | not (statusIsSuccessful (responseStatus resp)) ->
                Left ("API " <> tshow (statusCode (responseStatus resp)) <> ": " <> decodeLenient (BL.toStrict (responseBody resp)))
            | otherwise -> case eitherDecode (responseBody resp) of
                Left err -> Left (T.pack err)
                Right (RawModels ms) ->
                    Right . sortOn modelId $
                        [ ModelInfo i (fmap ("tools" `elem`) sp)
                        | RawModel i sp <- ms
                        , not (any (`T.isInfixOf` i) nonChatWords)
                        ]
