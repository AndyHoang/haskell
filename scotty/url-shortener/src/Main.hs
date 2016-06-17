{-# LANGUAGE OverloadedStrings #-}

module Main where
import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BC
import qualified Data.List.NonEmpty as NE
import Data.Semigroup
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Database.Redis as R
import Network.URI (parseURIReference)
import qualified System.Random as SR
import Web.Scotty

alphaNum :: NE.NonEmpty Char
alphaNum = NE.fromList ['A'..'Z'] <> NE.fromList ['0'..'9']

connectInfo :: R.ConnectInfo
connectInfo = R.defaultConnectInfo {R.connectHost = "192.168.99.100"}


randomElement :: NE.NonEmpty a -> IO a
randomElement xs = do
  let maxIndex = NE.length xs -1
  randomDigit <- SR.randomRIO (0, maxIndex)
  return (xs NE.!! randomDigit)

shortyGen :: Int -> IO String
shortyGen n = replicateM n $ randomElement alphaNum

saveSourceURI :: R.Connection -> BC.ByteString -> BC.ByteString -> IO (Either R.Reply R.Status)
saveSourceURI conn shortURI uri = R.runRedis conn $ R.set shortURI uri


getSourceURI :: R.Connection -> BC.ByteString -> IO (Either R.Reply (Maybe BC.ByteString))
getSourceURI conn shortURI = R.runRedis conn $ R.get shortURI


main :: IO ()
main = do
  rConn <- R.connect connectInfo
  scotty 3000 $ do
    --http://localhost:3000/?uri=facebook.com
    get "/" $ do
      uri <- param "uri"
      case parseURIReference (TL.unpack uri) of
        Just _ -> do
          short <- liftIO $ shortyGen 7
          let shortURI = BC.pack short
          resp <- liftIO (saveSourceURI rConn shortURI (encodeUtf8 (TL.toStrict uri)))
          text $ TL.concat [TL.pack (show resp), "shorty is: ", TL.pack short]
        Nothing -> text (TL.concat [uri, "wasn't a url"])
    get "/:short" $ do
      shortURI <- param "short"
      uri <- liftIO (getSourceURI rConn shortURI)
      case uri of
        Left reply -> text $ TL.pack (show reply)
        Right mbBS -> case mbBS of
          Nothing -> text "uri not found"
          Just bs -> html $ TL.concat ["<a href=\"", tbs, "\">", tbs, "</a>"]
            where tbs = TL.fromStrict (decodeUtf8 bs)

