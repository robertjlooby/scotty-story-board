{-# LANGUAGE OverloadedStrings #-}

module AppSpec (spec) where

import App (app)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Wai (get, matchStatus, shouldRespondWith, with)
import qualified Web.Scotty as S

spec :: Spec
spec = with (S.scottyApp app) $ do
  describe "GET /" $ do
    it "responds with 200 / 'hello'" $ do
      get "/" `shouldRespondWith` "hello" { matchStatus = 200 }
