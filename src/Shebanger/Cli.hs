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
import Options.Applicative
import Paths_shebanger (version)

data TranslateArgs = TranslateArgs { scriptFilePath :: FilePath }
  deriving stock Show

data ExecArgs = ExecArgs { shebangScriptPart :: ByteString }
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
execArgsParser = ExecArgs <$> shebangedScriptParser

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
