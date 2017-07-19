{-# LANGUAGE OverloadedStrings #-}

module Index where

import qualified IndexViews
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Web.Scotty as S


app :: S.ScottyM ()
app = do
    S.get "/" $ do
        S.html $ renderHtml $ IndexViews.index
