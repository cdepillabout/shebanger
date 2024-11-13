module Shebanger where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.ByteString.Base64 (encode)
import Data.Foldable (for_)
import Data.List (isSuffixOf)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Shebanger.Cli (Command (..), ExecArgs (..), TranslateArgs (..), parseCliOpts)
import System.Directory (doesFileExist)
import System.FilePath (takeFileName, (<.>), (-<.>))
import System.Posix (setFileMode, fileMode, getFileStatus, unionFileModes, ownerExecuteMode, groupExecuteMode, otherExecuteMode)
import System.Posix.Process (executeFile)
import System.Environment (lookupEnv, setEnv)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)

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
  -- TODO: This is assuming the script is utf8.  That's probably not a reasonable assumption???
  let shebangScriptPartStr = Text.unpack $ decodeUtf8 execArgs.shebangScriptPart
  print execArgs
  -- collect the script contents into an environment variable
  maybeShebangerScriptContents <- lookupEnv "SHEBANGER_SCRIPT_CONTENTS"
  print maybeShebangerScriptContents
  case getShebangedIndex execArgs.shebangScriptFilePath of
    Left badIndex ->
      error $
        "ERROR! Failed when trying to parse the current script's shebang index.  From file path \"" <>
        execArgs.shebangScriptFilePath <>
        "\", failed when trying to parse index: " <>
        badIndex
    Right idx -> do
      print idx
      maybeNextScript <- findNextScript execArgs.shebangScriptFilePath idx
      print maybeNextScript
      putStrLn ""
      case maybeNextScript of
        -- There is a next script that exists.  Update env var and execute the next script.
        Just nextScript -> do
          case maybeShebangerScriptContents of
            Nothing ->
              if idx == 0
              then
                setEnv "SHEBANGER_SCRIPT_CONTENTS" shebangScriptPartStr
              else error "ERROR! This is not the first script, but no SHEBANGER_SCRIPT_CONTENTS env var was found."
            Just envVarScriptContents ->
              setEnv "SHEBANGER_SCRIPT_CONTENTS" (envVarScriptContents <> shebangScriptPartStr)
          -- exec the next script
          executeFile nextScript True [] Nothing
        -- There is not a next script.  This script is the last script.
        Nothing -> do
          let fullScript = fromMaybe "" maybeShebangerScriptContents <> shebangScriptPartStr
          -- TODO: write the script out and execute it:
          print fullScript

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
