{-# LANGUAGE OverloadedStrings #-}

module OAuthLoginSpec (spec) where

import           Database.PostgreSQL.Simple (Connection)
import           Test.Hspec (Spec, describe, it, shouldBe)

import qualified OAuthLogin as O
import qualified User as U

spec :: Connection -> Spec
spec conn = describe "OAuthLogin" $ do
    it "can find user by id and provider" $ do
        userId <- U.create conn "user" "email"
        _ <- O.create conn userId "google" "12345"
        found <- O.findUser conn "google" "12345"
        found `shouldBe` (Just $ U.User userId "user" "email")

    it "returns nothing if not found" $ do
        found <- O.findUser conn "google" "12345"
        found `shouldBe` Nothing
