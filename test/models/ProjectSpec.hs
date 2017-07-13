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
        found `shouldBe` (Just $ P.Project projectId "project" "description")
    it "returns nothing if id not found" $ do
        found <- P.find conn (P.ProjectId 0)
        found `shouldBe` Nothing
    it "can find by name" $ do
        projectId <- P.create conn "project" "description"
        found <- P.findByName conn "project"
        found `shouldBe` (Just $ P.Project projectId "project" "description")
    it "returns nothing if name not found" $ do
        found <- P.findByName conn "project"
        found `shouldBe` Nothing
    it "can find all projects" $ do
        project1Id <- P.create conn "project1" ""
        project2Id <- P.create conn "project2" ""
        found <- P.findAll conn
        found `shouldBe` [P.Project project1Id "project1" "", P.Project project2Id "project2" ""]
    it "can delete by id" $ do
        projectId <- P.create conn "project" "description"
        _ <- P.delete conn projectId
        found <- P.find conn projectId
        found `shouldBe` Nothing
