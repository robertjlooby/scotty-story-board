{-# LANGUAGE OverloadedStrings #-}

module IndexSpec (spec) where

import Index (app)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Wai (get, shouldRespondWith, with)
import qualified Web.Scotty as S

spec :: Spec
spec = with (S.scottyApp app) $ do
    describe "GET /" $ do
        it "responds with 200" $ do
            get "/" `shouldRespondWith` 200
