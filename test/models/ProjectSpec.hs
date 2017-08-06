{-# LANGUAGE OverloadedStrings #-}

module ProjectSpec (spec) where

import           Database.PostgreSQL.Simple (Connection)
import           Test.Hspec (Spec, describe, it, shouldBe)

import qualified Project as P
import qualified User as U

spec :: Connection -> Spec
spec conn = describe "Project" $ do
    it "can find by id" $ do
        project <- P.create conn "project" "description"
        found <- P.find conn (P.id_ project)
        found `shouldBe` Just project

    it "returns nothing if id not found" $ do
        found <- P.find conn (P.ProjectId 0)
        found `shouldBe` Nothing

    it "can find by name" $ do
        project <- P.create conn "project" "description"
        found <- P.findByName conn "project"
        found `shouldBe` Just project

    it "returns nothing if name not found" $ do
        found <- P.findByName conn "project"
        found `shouldBe` Nothing

    it "can find by user id" $ do
        userId <- U.create conn "user" "email"
        project <- P.create conn "project1" "description"
        _ <- P.addUser conn (P.id_ project) userId

        otherUserId <- U.create conn "user2" "email2"
        otherProject <- P.create conn "project2" "description"

        found <- P.findByUserId conn userId (P.id_ project)
        found `shouldBe` Just project
        forOtherUser <- P.findByUserId conn otherUserId (P.id_ project)
        forOtherUser `shouldBe` Nothing
        forOtherProject <- P.findByUserId conn userId (P.id_ otherProject)
        forOtherProject `shouldBe` Nothing

    it "can find all by user id" $ do
        userId <- U.create conn "user" "email"
        project1 <- P.create conn "project1" "description"
        _ <- P.create conn "project2" "description"
        project3 <- P.create conn "project3" "description"
        _ <- P.addUser conn (P.id_ project1) userId
        _ <- P.addUser conn (P.id_ project3) userId
        found <- P.allByUserId conn userId
        found `shouldBe` [project1, project3]

    it "can update a project" $ do
        project <- P.create conn "name" "desc"
        let updated = P.Project (P.id_ project) "new name" "new desc"
        _ <- P.update conn updated
        found <- P.find conn (P.id_ project)
        found `shouldBe` (Just updated)

    it "can delete by id" $ do
        project <- P.create conn "project" "description"
        _ <- P.delete conn (P.id_ project)
        found <- P.find conn (P.id_ project)
        found `shouldBe` Nothing
