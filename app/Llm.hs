{-# LANGUAGE DuplicateRecordFields #-}

module Llm where

import Data.Aeson (Value)
import Data.Text qualified as T

data Model = Model
    { apiKey :: T.Text
    , model :: T.Text
    , baseUrl :: Maybe T.Text
    , maxTokens :: Maybe Int
    }

data Role
    = User
    | Assistant

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

data MessageContent
    = TextContent T.Text
    | BlocksContent [ContentBlock]

data Message = Message
    { role :: Role
    , content :: MessageContent
    }

data Context = Context
    { systemPrompt :: Maybe T.Text
    , messages :: [Message]
    }

data StopReason
    = EndTurn
    | ToolUse
    | MaxTokens
    | Aborted

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

data ToolDef = ToolDef
    { name :: T.Text
    , description :: T.Text
    , parameters :: Value
    }

stream :: Model -> Context -> Maybe ToolDef -> (T.Text -> IO ()) -> IO ()
stream md ctx td handler = pure ()
