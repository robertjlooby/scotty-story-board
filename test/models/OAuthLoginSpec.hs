{-# LANGUAGE OverloadedStrings #-}

module OAuthLoginSpec (spec) where

import           Test.Hspec (Spec, describe, it, shouldBe)

import           AppContext (HasDbConn(..))
import qualified OAuthLogin as O
import qualified User as U

spec :: HasDbConn a => a -> Spec
spec context = describe "OAuthLogin" $ do
    let conn = getDbConn context
    it "can find user by id and provider" $ do
        user <- U.create conn "user" "email"
        _ <- O.create conn (U.id_ user) "google" "12345"
        found <- O.findUser conn "google" "12345"
        found `shouldBe` Just user

    it "returns nothing if not found" $ do
        found <- O.findUser conn "google" "12345"
        found `shouldBe` Nothing
