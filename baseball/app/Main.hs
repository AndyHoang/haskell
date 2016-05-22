module Main where

import Lib
import System.Environment
main :: IO ()
main = do
  commands <- getArgs
  executeAction commands

executeAction:: [String] -> IO()
executeAction [] = sumStatsStream
executeAction (x:_) = do
  let (Just action) = lookup x dispatch
  action

dispatch :: [(String, IO())]
dispatch = [("default", sumStatsStream)
          , ("no-stream", sumStats)]
