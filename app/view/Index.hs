{-# LANGUAGE OverloadedStrings #-}

module Index (index) where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes

index :: Html
index = docTypeHtml $ do
  H.head $ do
    link ! rel "stylesheet" ! href "normalize.css"
    link ! rel "stylesheet" ! href "skeleton.css"
    H.title "Scotty Story Board"
  body $ do
    p "hello"
