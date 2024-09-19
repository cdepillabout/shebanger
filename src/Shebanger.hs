module Shebanger where

import Shebanger.Cli (parseCliOpts)
import Shebanger.Cmd (runCmd)
import Shebanger.LocalConfFile (readLocalConfFile)

defaultMain :: IO ()
defaultMain = do
  cmd <- parseCliOpts
  runCmd cmd
