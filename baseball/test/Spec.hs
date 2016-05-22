module Main where

import Lib
import Test.Hspec


main :: IO ()
main = hspec $ do
  describe "verify it can run" $ do
    it "equal here" $ do
      theSum <- sumStatsStream
      theSum `shouldBe` 4858210
