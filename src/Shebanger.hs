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
import System.Posix.ByteString (getEnv, setEnv, unsetEnv)
import System.Process (callProcess)
import Text.Read (readMaybe)

-- $setup
--
-- We need things from QuickCheck for some of the tests.
--
-- >>> import Test.QuickCheck

defaultMain :: IO ()
defaultMain = do
  cmd <- parseCliOpts
  runCmd cmd

runCmd :: Command -> IO ()
runCmd = \case
  Translate transArgs -> runCmdTranslate transArgs
  Exec execArgs -> runCmdExec execArgs

-- | The length of chunks of the input script.
--
-- The input script will be chunked into parts of this many bytes, then base-64
-- encoded (which increases the length by about 33%).  This base-64-encoded string
-- will then be put into the shebang line of all the shebanged scripts.
--
-- Note that Linux has a limit on how long a shebang line can be, so in practice this
-- has to be below 150 characters or so.
inputScriptChunkLength :: Int
inputScriptChunkLength = 50

runCmdTranslate :: TranslateArgs -> IO ()
runCmdTranslate transArgs = do
  let inputScriptFileName = takeFileName transArgs.scriptFilePath
  inputScriptContents <- ByteString.readFile transArgs.scriptFilePath
  let chunkedInputScript = chunkByteString inputScriptChunkLength inputScriptContents
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
          executeFile nextScript True execArgs.additionalArgs Nothing
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

          -- unset the SHEBANGER_SCRIPT_CONTENTS env var, since we don't want
          -- it to be inherited by the child.
          unsetEnv "SHEBANGER_SCRIPT_CONTENTS"

          -- Call the new executable, making sure to unlink it afterwards.
          finally
            (callProcess finalScriptName execArgs.additionalArgs)
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

-- | Split a list based on a predicate.
--
-- >>> split (== ' ') "hello world my name is bob"
-- "hello" :| ["world","my","name","is","bob"]
--
-- A predicate of @'const' 'True'@ splits on everything, leaving you with
-- a list of empty lists, with one more entry than your original list:
--
-- >>> split (const True) "bye"
-- "" :| ["","",""]
--
-- A predicate of @'const' 'False'@ produces no splits:
--
-- >>> split (const False) "bye"
-- "bye" :| []
--
-- An empty list doesn't get split, regardless of the predicate:
--
-- prop> \(Fun _ f) -> split f "" == ("" :| [])
split :: forall a. (a -> Bool) -> [a] -> NonEmpty [a]
split _ [] = [] :| []
split p t = loop t
  where
    loop :: [a] -> NonEmpty [a]
    loop s
      | null s'   = l :| []
      | otherwise = NonEmpty.cons l $ loop (tail s')
      where (l, s') = break p s
