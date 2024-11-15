module Shebanger.Cli
  ( parseCliOpts
  , Command (..)
  , TranslateArgs (..)
  , ExecArgs (..)
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.ByteString.Base64 (decode)
import Data.Version (showVersion)
import Control.Applicative ((<**>), Alternative (many, (<|>)))
import Options.Applicative
    ( action, argument, command, eitherReader, fullDesc, header, help, info,
      metavar, progDesc, strArgument, execParser, helper, hsubparser,
      simpleVersioner, CommandFields, Mod, Parser, ParserInfo
    )
import Paths_shebanger (version)

data TranslateArgs = TranslateArgs { scriptFilePath :: FilePath }
  deriving stock Show

data ExecArgs = ExecArgs
  { shebangScriptPart :: ByteString
    -- ^ The base64-encoded part of the original script.
    --
    -- Example: @\"ICAgZWNobyAkaQpkb25lCmVjaG8KCiMgTGlzdCBzeXN0ZW0gaW5mb3JtYXRpb24KdW4=\"@
  , shebangScriptFilePath :: FilePath
    -- ^ The path of this shebanged script.  This is normally passed
    -- automatically by the kernel when calling a script with a shebang line.
    --
    -- Example: @\"./test.sh.shebanged.7\"@
  , additionalArgs :: [String]
    -- ^ Additional arguments that have been passed to the script on the command line.
    --
    -- Example @[\"-l\", \"-a\", \"somedirectory/\"]@
  }
  deriving stock Show

data Command
  = Translate TranslateArgs
  | Exec ExecArgs
  deriving stock Show

parseCliOpts :: IO Command
parseCliOpts = execParser cliCmdParserInfo

cliCmdParserInfo :: ParserInfo Command
cliCmdParserInfo =
  info
    ( cliCmdParser <**>
      helper <**>
      simpleVersioner (showVersion version)
    )
    ( fullDesc <>
      header "shebanger - translate a shell script into a series of shebang lines"
    )

cliCmdParser :: Parser Command
cliCmdParser = hsubparser (translateCommandMod <> execCommandMod) <|> translateCommandParser
  where
    translateCommandMod :: Mod CommandFields Command
    translateCommandMod =
      command
        "translate"
        ( info
            translateCommandParser
            (progDesc "Translate a shell script into a series of scripts with only shebang lines")
        )

    execCommandMod :: Mod CommandFields Command
    execCommandMod =
      command
        "exec"
        ( info
            (fmap Exec execArgsParser)
            (progDesc "Execute shebanged shell scripts")
        )

    translateCommandParser :: Parser Command
    translateCommandParser = fmap Translate translateArgsParser

translateArgsParser :: Parser TranslateArgs
translateArgsParser = TranslateArgs <$> inputFileParser

inputFileParser :: Parser FilePath
inputFileParser =
  strArgument
    ( metavar "FILE" <>
      help "Input shell scripts to shebang" <>
      action "file"
    )

execArgsParser :: Parser ExecArgs
execArgsParser =
  ExecArgs
    <$> shebangedScriptParser
    <*> shebangScriptFilePathParser
    <*> additionalArgsParser


shebangedScriptParser :: Parser ByteString
shebangedScriptParser =
  argument
    (eitherReader b64Reader)
    ( metavar "SHEBANG_SCRIPT_PART" <>
      help "Part of a shebang translated script file.  Expected to be in base 64."
    )
  where
    b64Reader :: String -> Either String ByteString
    b64Reader b64Str = decode $ ByteString.Char8.pack b64Str

shebangScriptFilePathParser :: Parser FilePath
shebangScriptFilePathParser =
  strArgument
    ( metavar "FILE" <>
      help "Input shebanged script name.  Normally passed automatically by kernel." <>
      action "file"
    )

additionalArgsParser :: Parser [String]
additionalArgsParser =
  many $
    strArgument
      ( metavar "ARG" <>
        help "arguments to pass to the underlying script being called"
      )
