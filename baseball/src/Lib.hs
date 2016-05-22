module Lib
  ( sumStats, sumStatsStream
    ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
-- cassava
import Data.Csv as CSV
import Data.Csv.Streaming as CSVS
import Data.Foldable as F
type BaseballStats = (BL.ByteString, Int, BL.ByteString, Int)


sumStats :: IO Int
sumStats  = do
  csvData <- BL.readFile "batting.csv"
  let v = CSV.decode NoHeader csvData :: Either String (V.Vector BaseballStats)
      summed = fmap (V.foldr ((+).fourth) 0) v
   in case summed of Left _ -> return 0
                     Right sum -> return sum
--  putStrLn $ "Total atBats was: " ++ (show summed)



fourth :: (a,b,c,d) -> d
fourth (_,_,_,d) = d

sumStatsStream :: IO Int
sumStatsStream = do
  csvData <- BL.readFile "batting.csv"
  let v = CSVS.decode NoHeader csvData ::CSVS.Records BaseballStats
  return $ F.foldr ((+).fourth) 0 v
  --putStrLn $ "Total atBats was: " ++ (show summed)

