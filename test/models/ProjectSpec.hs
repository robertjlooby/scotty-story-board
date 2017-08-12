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
        user <- U.create conn "user" "email"
        project <- P.create conn "project1" "description"
        _ <- P.addUser conn (P.id_ project) (U.id_ user)

        otherUser <- U.create conn "user2" "email2"
        otherProject <- P.create conn "project2" "description"

        found <- P.findByUserId conn (U.id_ user) (P.id_ project)
        found `shouldBe` Just project
        forOtherUser <- P.findByUserId conn (U.id_ otherUser) (P.id_ project)
        forOtherUser `shouldBe` Nothing
        forOtherProject <- P.findByUserId conn (U.id_ user) (P.id_ otherProject)
        forOtherProject `shouldBe` Nothing

    it "can find all by user id" $ do
        user <- U.create conn "user" "email"
        project1 <- P.create conn "project1" "description"
        _ <- P.create conn "project2" "description"
        project3 <- P.create conn "project3" "description"
        _ <- P.addUser conn (P.id_ project1) (U.id_ user)
        _ <- P.addUser conn (P.id_ project3) (U.id_ user)
        found <- P.allByUserId conn (U.id_ user)
        found `shouldBe` [project1, project3]

    it "can update a project" $ do
        project <- P.create conn "name" "desc"
        let updated = project {P.name = "new name", P.description = "new desc"}
        _ <- P.update conn updated
        found <- P.find conn (P.id_ project)
        found `shouldBe` (Just updated)

    it "can delete by id" $ do
        project <- P.create conn "project" "description"
        _ <- P.delete conn (P.id_ project)
        found <- P.find conn (P.id_ project)
        found `shouldBe` Nothing
