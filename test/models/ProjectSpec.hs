{-# LANGUAGE OverloadedStrings #-}

module ProjectSpec (spec) where

import Database.PostgreSQL.Simple (Connection)
import Project as P
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Connection -> Spec
spec conn = describe "Project" $ do
    it "can find by id" $ do
        projectId <- P.create conn "project" "description"
        found <- P.find conn projectId
        found `shouldBe` (Just $ P.Project "project" "description")
    it "returns nothing if id not found" $ do
        found <- P.find conn (P.ProjectId 0)
        found `shouldBe` Nothing
