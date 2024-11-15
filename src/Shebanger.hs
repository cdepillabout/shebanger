module Shebanger where

import Control.Exception (finally)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.ByteString.Base64 (encode)
import Data.Foldable (for_)
import Data.List (isSuffixOf)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe)
import Shebanger.Cli (Command (..), ExecArgs (..), TranslateArgs (..), parseCliOpts)
import System.Directory (doesFileExist, removeFile)
import System.FilePath (takeFileName, (<.>), (-<.>))
import System.Posix (setFileMode, fileMode, getFileStatus, unionFileModes, ownerExecuteMode, groupExecuteMode, otherExecuteMode)
import System.Posix.Process (executeFile)
import System.Posix.ByteString (getEnv, setEnv)
import System.Process (callProcess)
import Text.Read (readMaybe)

defaultMain :: IO ()
defaultMain = do
  cmd <- parseCliOpts
  runCmd cmd

runCmd :: Command -> IO ()
runCmd = \case
  Translate transArgs -> runCmdTranslate transArgs
  Exec execArgs -> runCmdExec execArgs

runCmdTranslate :: TranslateArgs -> IO ()
runCmdTranslate transArgs = do
  let inputScriptFileName = takeFileName transArgs.scriptFilePath
  inputScriptContents <- ByteString.readFile transArgs.scriptFilePath
  let chunkedInputScript = chunkByteString 50 inputScriptContents
      b64ChunkedInputScript = fmap encode chunkedInputScript
  writeShebanged inputScriptFileName b64ChunkedInputScript

writeShebanged :: String -> [ByteString] -> IO ()
writeShebanged scriptName b64Chunks =
  for_ (zip [0..] b64Chunks) $ \(i :: Int, chunk) -> do
    let fnameBase = scriptName <.> "shebanged"
        fname = if i == 0 then fnameBase else fnameBase <.> show i
        scriptContents = "#!/usr/bin/env -S shebanger exec " <> chunk
    ByteString.writeFile fname scriptContents
    makeExecutable fname

makeExecutable :: FilePath -> IO ()
makeExecutable path = do
    -- Get the current file permissions
    status <- getFileStatus path
    let currentMode = fileMode status
    -- Define the new mode with executable permissions
    let newMode = currentMode `unionFileModes` ownerExecuteMode `unionFileModes` groupExecuteMode `unionFileModes` otherExecuteMode
    -- Set the file mode to the new mode
    setFileMode path newMode

chunkByteString :: Int -> ByteString -> [ByteString]
chunkByteString n bs
    | ByteString.null bs = []
    | otherwise =
        let (chunk, rest) = ByteString.splitAt n bs
        in chunk : chunkByteString n rest

runCmdExec :: ExecArgs -> IO ()
runCmdExec execArgs = do
  maybeShebangerScriptContents <- getEnv "SHEBANGER_SCRIPT_CONTENTS"
  case getShebangedIndex execArgs.shebangScriptFilePath of
    Left badIndex ->
      error $
        "ERROR! Failed when trying to parse the current script's " <>
        "shebang index.  From file path \"" <>
        execArgs.shebangScriptFilePath <>
        "\", failed when trying to parse index: " <>
        badIndex
    Right idx -> do
      maybeNextScript <- findNextScript execArgs.shebangScriptFilePath idx
      case maybeNextScript of
        -- There is a next script that exists.  Update env var and execute the
        -- next script.
        Just nextScript -> do
          case maybeShebangerScriptContents of
            Nothing ->
              if idx == 0
              then
                -- This is the initial script (script.sh.shebanger), so we
                -- expect that there is not yet a SHEBANGER_SCRIPT_CONTENTS
                -- env var.  We create the env var with the initial part of
                -- the script.
                setEnv "SHEBANGER_SCRIPT_CONTENTS" execArgs.shebangScriptPart True
              else
                -- This is not the initial script (so it has a name like
                -- script.sh.shebanger.07), but there is no
                -- SHEBANGER_SCRIPT_CONTENTS env var.  This is unexpected.
                error $
                  "ERROR! This is not the first script, but no " <>
                  "SHEBANGER_SCRIPT_CONTENTS env var was found."
            Just envVarScriptContents ->
              setEnv
                "SHEBANGER_SCRIPT_CONTENTS"
                (envVarScriptContents <> execArgs.shebangScriptPart)
                True
          -- exec the next script
          executeFile nextScript True [] Nothing
        -- There is not a next script.  This script is the last script.
        Nothing -> do
          let fullScript =
                fromMaybe "" maybeShebangerScriptContents <> execArgs.shebangScriptPart
              finalScriptName =
                if idx == 0
                then execArgs.shebangScriptFilePath <.> "final"
                else execArgs.shebangScriptFilePath -<.> "final"

          ByteString.writeFile finalScriptName fullScript
          makeExecutable finalScriptName

          -- Call the new executable, making sure to unlink it afterwards.
          finally
            (callProcess finalScriptName [])
            (removeFile finalScriptName)

findNextScript :: FilePath -> Int -> IO (Maybe FilePath)
findNextScript fp currIdx = do
  let nextIdx = currIdx + 1
      nextScriptName =
        -- The initial script just ends with `.shebanged` (which we consider
        -- index 0), so in that case just add ".1".  Otherwise, increment the
        -- index by one.
        if currIdx == 0
        then fp <.> ".1"
        else fp -<.> show nextIdx
  exists <- doesFileExist nextScriptName
  pure $ if exists then Just nextScriptName else Nothing

-- | Returns 'Left' 'String' of the last part of the filename that it is trying
-- to parse a number on error.
--
-- Returns 'Right' 'Int' with the index if it is found correctly.
getShebangedIndex :: FilePath -> Either String Int
getShebangedIndex fname =
  if ".shebanged" `isSuffixOf` fname
  -- The .shebanged file is the initial file that represents index 0
  then Right 0
  else do
    -- The shebanged filenames are going to look like:
    -- my-script.sh.shebanged.XX, where XX is a number.
    let splitFname = split (== '.') fname
        -- pull out just the number
        strNum = NonEmpty.last splitFname
        -- try to parse it as a number
        maybeNum = readMaybe strNum
    case maybeNum of
      -- could not read file name part as number
      Nothing -> Left strNum
      Just num
        -- the file name part should never be less than 1
        | num < 1 -> Left strNum
        | otherwise -> Right num

split :: forall a. (a -> Bool) -> [a] -> NonEmpty [a]
split _ [] = [] :| []
split p t = loop t
  where
    loop :: [a] -> NonEmpty [a]
    loop s
      | null s'   = l :| []
      | otherwise = NonEmpty.cons l $ loop (tail s')
      where (l, s') = break p s
