
module Main where

import Test.Shebanger (shebangerTests)
import Test.Tasty (defaultMain)

main :: IO ()
main = defaultMain shebangerTests
