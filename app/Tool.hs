{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Tool (Tool (..), ToolResult, tools) where

import Data.Aeson (FromJSON (..), Result (Error, Success), Value, fromJSON, object, withObject, (.:), (.:?), (.=))
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (createDirectoryIfMissing, doesFileExist, getTemporaryDirectory)
import System.FilePath (takeDirectory)
import System.IO (hClose, openTempFile)
import System.Process (readCreateProcessWithExitCode, shell)

data ToolResult
    = ToolSuccess T.Text
    | ToolError T.Text
    deriving (Show, Eq)

data Tool = Tool
    { name :: T.Text
    , description :: T.Text
    , parameters :: Value
    , execute :: Value -> IO ToolResult
    }

readFileSchema :: Value
readFileSchema =
    object
        [ "type" .= ("object" :: String)
        , "properties"
            .= object
                [ "path"
                    .= object
                        [ "type" .= ("string" :: String)
                        , "description" .= ("Path to the file to read" :: String)
                        ]
                , "offset"
                    .= object
                        [ "type" .= ("integer" :: String)
                        , "description" .= ("Line number to start reading from (1-based)" :: String)
                        ]
                , "limit"
                    .= object
                        [ "type" .= ("integer" :: String)
                        , "description" .= ("Maximum number of lines to read" :: String)
                        ]
                ]
        , "required" .= (["path"] :: [String])
        ]

data ReadFileArgs = ReadFileArgs
    { filePath :: FilePath
    , fileOffset :: Maybe Int
    , fileLimit :: Maybe Int
    }
    deriving (Show)

instance FromJSON ReadFileArgs where
    parseJSON = withObject "ReadFileArgs" $ \obj -> do
        p <- obj .: "path"
        off <- obj .:? "offset"
        lim <- obj .:? "limit"
        pure
            ReadFileArgs
                { filePath = p
                , fileOffset = off
                , fileLimit = lim
                }

formatLines :: (Int, T.Text) -> T.Text
formatLines (n, line) =
    let numStr = T.justifyRight 6 ' ' (T.pack $ show n)
     in numStr <> "\t" <> line

readFileExecute :: Value -> IO ToolResult
readFileExecute val = case fromJSON val of
    Error err -> pure $ ToolError $ T.pack $ "Invalid arguments:  " ++ err
    Success (ReadFileArgs path offset limit) -> do
        exists <- doesFileExist path
        if not exists
            then pure $ ToolError $ T.pack $ "File not found: " ++ path
            else do
                content <- TIO.readFile path
                let allLines = T.lines content
                let startLine = case offset of
                        Just n -> max 1 n
                        Nothing -> 1
                let offsetContent = drop (startLine - 1) allLines
                let finalLines = case limit of
                        Just lim -> take lim offsetContent
                        Nothing -> offsetContent
                let numbered = zip [startLine ..] finalLines
                pure $ ToolSuccess $ T.unlines $ map formatLines numbered

readFileTool :: Tool
readFileTool =
    Tool
        { name = "read_file"
        , description = "reads the file"
        , parameters = readFileSchema
        , execute = readFileExecute
        }

writeFileSchema :: Value
writeFileSchema =
    object
        [ "type" .= ("object" :: String)
        , "properties"
            .= object
                [ "path"
                    .= object
                        [ "type" .= ("string" :: String)
                        , "description" .= ("Path to the file to write" :: String)
                        ]
                , "content"
                    .= object
                        [ "type" .= ("string" :: String)
                        , "description" .= ("Content that need to be written in file" :: String)
                        ]
                ]
        , "required" .= (["path", "content"] :: [String])
        ]

data WriteFileArgs = WriteFileArgs
    { filePath :: FilePath
    , content :: T.Text
    }
    deriving (Show)

instance FromJSON WriteFileArgs where
    parseJSON = withObject "WriteFileArgs" $ \obj -> do
        p <- obj .: "path"
        c <- obj .: "content"
        pure
            WriteFileArgs
                { filePath = p
                , content = c
                }

writeFileExecute :: Value -> IO ToolResult
writeFileExecute val = case fromJSON val of
    Error err -> pure $ ToolError $ T.pack $ "Invalid arguments: " ++ err
    Success (WriteFileArgs filePath content) -> do
        createDirectoryIfMissing True $ takeDirectory filePath
        TIO.writeFile filePath content
        pure $ ToolSuccess $ T.pack $ "Written " ++ show (T.length content) ++ " to the file: " ++ filePath

writeFileTool :: Tool
writeFileTool =
    Tool
        { name = "write_file"
        , description = "Create or fully overwrite a file (parent dirs are created). For small changes to existing files use edit."
        , parameters = writeFileSchema
        , execute = writeFileExecute
        }

editFileSchema :: Value
editFileSchema =
    object
        [ "type" .= ("object" :: String)
        , "properties"
            .= object
                [ "path"
                    .= object
                        [ "type" .= ("string" :: String)
                        , "description" .= ("Path to the file to edit" :: String)
                        ]
                , "old_content"
                    .= object
                        [ "type" .= ("string" :: String)
                        , "description" .= ("Content that need to be replaced" :: String)
                        ]
                , "new_content"
                    .= object
                        [ "type" .= ("string" :: String)
                        , "description" .= ("Content that need to be added" :: String)
                        ]
                ]
        , "required" .= (["path", "old_content", "new_content"] :: [String])
        ]

data EditFileArgs = EditFileArgs
    { filePath :: FilePath
    , oldContent :: T.Text
    , newContent :: T.Text
    }
    deriving (Show)

instance FromJSON EditFileArgs where
    parseJSON = withObject "EditFileArgs" $ \obj -> do
        p <- obj .: "path"
        oc <- obj .: "old_content"
        nc <- obj .: "new_content"
        pure
            EditFileArgs
                { filePath = p
                , oldContent = oc
                , newContent = nc
                }

editFileExecute :: Value -> IO ToolResult
editFileExecute val = case fromJSON val of
    Error err -> pure $ ToolError $ T.pack $ "Invalid arguments: " ++ err
    Success (EditFileArgs filePath oldContent newContent) -> do
        exists <- doesFileExist filePath
        if not exists
            then pure $ ToolError $ T.pack $ "File not found: " ++ filePath
            else do
                content <- TIO.readFile filePath
                case T.count oldContent content of
                    0 -> pure $ ToolError "old_content not found"
                    1 -> do
                        let c = T.replace oldContent newContent content
                        TIO.writeFile filePath c
                        pure $ ToolSuccess $ T.pack $ "edited the file " ++ filePath
                    n -> pure $ ToolError $ "old_content occurs " <> T.pack (show n) <> " times; must be unique"

editFileTool :: Tool
editFileTool =
    Tool
        { name = "edit_file"
        , description = "Edit a file. Replace the unique old_content occurance with new_content"
        , parameters = editFileSchema
        , execute = editFileExecute
        }

runBashSchema :: Value
runBashSchema =
    object
        [ "type" .= ("object" :: String)
        , "properties"
            .= object
                [ "command"
                    .= object
                        [ "type" .= ("string" :: String)
                        , "description" .= ("Bash command to run" :: String)
                        ]
                ]
        , "required" .= (["command"] :: [String])
        ]

newtype RunBashFileArgs = RunBashFileArgs {command :: T.Text} deriving (Show)

instance FromJSON RunBashFileArgs where
    parseJSON = withObject "RunBashFileArgs" $ \obj -> do
        c <- obj .: "command"
        pure RunBashFileArgs{command = c}

truncateOutput :: T.Text -> Int -> IO T.Text
truncateOutput content lim
    | total <= lim = pure content
    | otherwise = do
        tmpDir <- getTemporaryDirectory
        (path, h) <- openTempFile tmpDir "output-.txt"
        TIO.hPutStr h content
        hClose h
        let lastN = T.unlines (drop (total - lim) ls)
        pure $
            T.concat
                [ "[output truncated to last :"
                , T.pack (show lim)
                , " lines]\n"
                , lastN
                , "\n Full output: file://"
                , T.pack path
                ]
  where
    ls = T.lines content
    total = length ls

runBashExecute :: Value -> IO ToolResult
runBashExecute val = case fromJSON val of
    Error err -> pure $ ToolError $ T.pack $ "Invalid arguments: " ++ err
    Success (RunBashFileArgs command) -> do
        (e, so, se) <- readCreateProcessWithExitCode (shell $ T.unpack command) ""
        let content = T.concat ["Exit code: ", T.pack $ show e, "\nStandard output: ", T.pack so, "\nStandard Error: ", T.pack se]
        finaloutput <- truncateOutput content 200
        pure $ ToolSuccess finaloutput

runBashTool :: Tool
runBashTool =
    Tool
        { name = "run_bash"
        , description = "Run a shell command in cwd. output over 200 lines is cut to the last 200 with the full log saved to a temp file."
        , parameters = runBashSchema
        , execute = runBashExecute
        }

tools :: [Tool]
tools = [readFileTool, writeFileTool, editFileTool, runBashTool]
