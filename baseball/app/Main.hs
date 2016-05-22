module Main where

import Lib
import System.Environment
main :: IO ()
main = do
  commands <- getArgs
  summed <- executeAction commands
  putStrLn $ "Total atBats was: " ++ (show summed)

executeAction:: [String] -> IO Int
executeAction [] = sumStatsStream
executeAction (x:_) = do
  let (Just action) = lookup x dispatch
  action

dispatch :: [(String, IO Int)]
dispatch = [("default", sumStatsStream)
          , ("no-stream", sumStats)]
