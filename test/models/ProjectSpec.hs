{-# LANGUAGE OverloadedStrings #-}

module ProjectSpec (spec) where

import           Database.PostgreSQL.Simple (Connection)
import           Test.Hspec (Spec, describe, it, shouldBe)

import qualified Project as P
import qualified User as U

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

    it "can find by user id" $ do
        userId <- U.create conn "user" "email"
        projectId <- P.create conn "project1" "description"
        (Just project) <- P.find conn projectId
        _ <- P.addUser conn projectId userId

        otherUserId <- U.create conn "user2" "email2"
        otherProjectId <- P.create conn "project2" "description"

        found <- P.findByUserId conn userId projectId
        found `shouldBe` Just project
        forOtherUser <- P.findByUserId conn otherUserId projectId
        forOtherUser `shouldBe` Nothing
        forOtherProject <- P.findByUserId conn userId otherProjectId
        forOtherProject `shouldBe` Nothing

    it "can find all by user id" $ do
        userId <- U.create conn "user" "email"
        projectId1 <- P.create conn "project1" "description"
        _ <- P.create conn "project2" "description"
        projectId3 <- P.create conn "project3" "description"
        (Just project1) <- P.find conn projectId1
        (Just project3) <- P.find conn projectId3
        _ <- P.addUser conn projectId1 userId
        _ <- P.addUser conn projectId3 userId
        found <- P.allByUserId conn userId
        found `shouldBe` [project1, project3]

    it "can update a project" $ do
        projectId <- P.create conn "name" "desc"
        let updated = P.Project projectId "new name" "new desc"
        _ <- P.update conn updated
        found <- P.find conn projectId
        found `shouldBe` (Just updated)

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
