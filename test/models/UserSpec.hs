{-# LANGUAGE OverloadedStrings #-}

module UserSpec (spec) where

import            Database.PostgreSQL.Simple (Connection)
import            Test.Hspec (Spec, describe, it, shouldBe)

import qualified  User as U

spec :: Connection -> Spec
spec conn = describe "User" $ do
    it "can find by id" $ do
        user <- U.create conn "user" "email"
        found <- U.find conn (U.id_ user)
        found `shouldBe` Just user

    it "returns nothing if id not found" $ do
        found <- U.find conn (U.UserId 0)
        found `shouldBe` Nothing

    it "can find by name" $ do
        user <- U.create conn "user" "email"
        found <- U.findByName conn "user"
        found `shouldBe` Just user

    it "returns nothing if name not found" $ do
        found <- U.findByName conn "user"
        found `shouldBe` Nothing
