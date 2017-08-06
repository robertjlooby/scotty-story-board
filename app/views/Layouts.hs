{-# LANGUAGE OverloadedStrings #-}

module Layouts (app) where

import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes
import qualified Web.Scotty as S

app :: Html -> S.ActionM ()
app pageBody = S.html . renderHtml . docTypeHtml $ do
    H.head $ do
        link ! rel "stylesheet" ! href "/normalize.css"
        link ! rel "stylesheet" ! href "/skeleton.css"
        H.title "Scotty Story Board"
    body pageBody
