{-# LANGUAGE OverloadedStrings #-}

module App where

import qualified Web.Scotty as S

app :: S.ScottyM ()
app = do
  S.get "/" $ do
    S.text "hello"
