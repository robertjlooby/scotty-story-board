{-# LANGUAGE OverloadedStrings #-}

module Index where

import           Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Web.Scotty as S

import qualified IndexViews

app :: S.ScottyM ()
app = do
    S.get "/" $ do
        S.html $ renderHtml $ IndexViews.index
