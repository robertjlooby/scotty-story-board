{-# LANGUAGE OverloadedStrings #-}

module IndexController where

import qualified Web.Scotty as S

import qualified IndexViews
import           Session (getSession)

app :: S.ScottyM ()
app = do
    S.get "/" $ getSession >>= IndexViews.index
