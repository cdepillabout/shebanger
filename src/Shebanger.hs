module Shebanger where

import Shebanger.Cli

defaultMain :: IO ()
defaultMain = do
  cmd <- parseCliOpts
  runCmd cmd

runCmd :: Command -> IO ()
runCmd _ = undefined
