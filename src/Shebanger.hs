module Shebanger where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.ByteString.Base64 (encode)
import Data.Foldable (for_)
import Data.List (isSuffixOf)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Shebanger.Cli (Command (..), ExecArgs (..), TranslateArgs (..), parseCliOpts)
import System.FilePath (takeFileName, (<.>))
import System.Posix (setFileMode, fileMode, getFileStatus, unionFileModes, ownerExecuteMode, groupExecuteMode, otherExecuteMode)
import System.Environment (getArgs, lookupEnv)
import Text.Read (readMaybe)

defaultMain :: IO ()
defaultMain = do
  args <- getArgs
  print args
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
  -- collect the script contents into an environment variable
  maybeShebangerScriptContents <- lookupEnv "SHEBANGER_SCRIPT_CONTENTS"
  case getShebangedIndex execArgs.shebangScriptFilePath of
    Left badIndex ->
      error $
        "ERROR! Failed when trying to parse the current script's shebang index.  From file path \"" <>
        execArgs.shebangScriptFilePath <>
        "\", failed when trying to parse index: " <>
        badIndex
    Right idx -> do
      print idx

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
