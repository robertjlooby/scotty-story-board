{-# LANGUAGE OverloadedStrings #-}

module UserSpec (spec) where

import           Test.Hspec (Spec, describe, it, shouldBe)

import           AppContext (HasDbConn(..))
import qualified User as U

spec :: HasDbConn a => a -> Spec
spec context = describe "User" $ do
    let conn = getDbConn context
    it "can find by id" $ do
        user <- U.create conn "user" "email"
        found <- U.runUserFindQuery conn $ U.findQuery (U.id_ user)
        found `shouldBe` Just user

    it "returns nothing if id not found" $ do
        found <- U.runUserFindQuery conn $ U.findQuery (U.UserId 0)
        found `shouldBe` Nothing

    it "can find by name" $ do
        user <- U.create conn "user" "email"
        found <- U.findByName conn "user"
        found `shouldBe` Just user

    it "returns nothing if name not found" $ do
        found <- U.findByName conn "user"
        found `shouldBe` Nothing
