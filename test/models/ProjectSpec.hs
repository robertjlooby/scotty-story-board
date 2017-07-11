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
    it "can find by name" $ do
        _ <- P.create conn "project" "description"
        found <- P.findByName conn "project"
        found `shouldBe` (Just $ P.Project "project" "description")
    it "returns nothing if name not found" $ do
        found <- P.findByName conn "project"
        found `shouldBe` Nothing
    it "can find all projects" $ do
        _ <- P.create conn "project1" ""
        _ <- P.create conn "project2" ""
        found <- P.findAll conn
        found `shouldBe` [P.Project "project1" "", P.Project "project2" ""]
