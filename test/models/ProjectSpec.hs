{-# LANGUAGE OverloadedStrings #-}

module ProjectSpec (spec) where

import           Test.Hspec (Spec, describe, it, shouldBe)

import            AppContext (HasDbConn(..))
import qualified Project as P
import qualified User as U

spec :: HasDbConn a => a -> Spec
spec context = describe "Project" $ do
    let conn = getDbConn context
    it "can find by id" $ do
        project <- P.create conn "project" "description"
        found <- P.find conn (P._projectId project)
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
        user <- U.create conn "user" "email"
        project <- P.create conn "project1" "description"
        _ <- P.addUser conn (P._projectId project) (U._userId user)

        otherUser <- U.create conn "user2" "email2"
        otherProject <- P.create conn "project2" "description"

        found <- P.findByUserId conn (U._userId user) (P._projectId project)
        found `shouldBe` Just project
        forOtherUser <- P.findByUserId conn (U._userId otherUser) (P._projectId project)
        forOtherUser `shouldBe` Nothing
        forOtherProject <- P.findByUserId conn (U._userId user) (P._projectId otherProject)
        forOtherProject `shouldBe` Nothing

    it "can find all by user id" $ do
        user <- U.create conn "user" "email"
        project1 <- P.create conn "project1" "description"
        _ <- P.create conn "project2" "description"
        project3 <- P.create conn "project3" "description"
        _ <- P.addUser conn (P._projectId project1) (U._userId user)
        _ <- P.addUser conn (P._projectId project3) (U._userId user)
        found <- P.allByUserId conn (U._userId user)
        found `shouldBe` [project1, project3]

    it "can update a project" $ do
        project <- P.create conn "name" "desc"
        let updated = project {P._projectName = "new name", P._projectDescription = "new desc"}
        _ <- P.update conn updated
        found <- P.find conn (P._projectId project)
        found `shouldBe` (Just updated)

    it "can delete by id" $ do
        project <- P.create conn "project" "description"
        _ <- P.delete conn (P._projectId project)
        found <- P.find conn (P._projectId project)
        found `shouldBe` Nothing
