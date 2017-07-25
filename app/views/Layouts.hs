{-# LANGUAGE OverloadedStrings #-}

module Layouts (app) where

import           Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes

app :: Html -> Html
app pageBody = docTypeHtml $ do
    H.head $ do
        link ! rel "stylesheet" ! href "/normalize.css"
        link ! rel "stylesheet" ! href "/skeleton.css"
        H.title "Scotty Story Board"
    body pageBody
