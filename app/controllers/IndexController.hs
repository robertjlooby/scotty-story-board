{-# LANGUAGE OverloadedStrings #-}

module IndexController where

import           Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Web.Scotty as S

import qualified IndexViews
import           Session (getSession)

app :: S.ScottyM ()
app = do
    S.get "/" $ do
        session <- getSession
        S.html $ renderHtml $ IndexViews.index session
