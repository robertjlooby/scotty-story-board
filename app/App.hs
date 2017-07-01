{-# LANGUAGE OverloadedStrings #-}

module App where

import qualified Index
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Web.Scotty as S

app :: S.ScottyM ()
app = do
  S.get "/" $ do
    S.html $ renderHtml Index.index
