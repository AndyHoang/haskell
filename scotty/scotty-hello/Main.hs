{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
import Web.Scotty
import Text.Blaze.Renderer.Utf8
import Text.Hamlet (shamletFile)

main :: IO ()
main = scotty 3001 $
  get "/:word".raw.renderMarkup $ ($(shamletFile "index.hamlet"))
