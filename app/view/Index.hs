{-# LANGUAGE OverloadedStrings #-}

module Index (index) where

import Text.Blaze.Html5 as H

index :: Html
index = docTypeHtml $ do
  H.head $ do
    title "Scotty Story Board"
  body $ do
    p "hello"
