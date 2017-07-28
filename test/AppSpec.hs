{-# LANGUAGE OverloadedStrings #-}

module AppSpec where

import qualified Data.ByteString.Char8 as BS
import           Data.Monoid ((<>))
import           Database.PostgreSQL.Simple (Connection)
import           Test.Hspec (Spec, describe, it, shouldBe)
import           Test.Hspec.Wai (WaiSession, delete, get, liftIO, post, put, shouldRespondWith, with)
import qualified Web.Scotty as S

import           App (app)
import           Helpers (delete', get', postHtmlForm', putHtmlForm', run, withSession)
import qualified Project as P
import           Session (Session(Session))
import qualified User as U

createUser :: Connection -> WaiSession (U.UserId, Session)
createUser conn = do
    userId <- liftIO $ U.create conn "user" "email"
    return (userId, Session userId)

spec :: Connection -> Spec
spec conn = with (S.scottyApp $ app conn) $ do
    describe "all projects routes respond with a 401 if not logged in" $ do
        it "GET /projects" $ do
            get "/projects" `shouldRespondWith` 401

        it "GET /projects/:id" $ do
            get "/projects/0" `shouldRespondWith` 401

        it "GET /projects/:id/edit" $ do
            get "/projects/0/edit" `shouldRespondWith` 401

        it "PUT /projects/:id" $ do
            put "/projects/0" "" `shouldRespondWith` 401

        it "DELETE /projects/:id" $ do
            delete "/projects/0" `shouldRespondWith` 401

        it "GET /projects/new" $ do
            get "/projects/new" `shouldRespondWith` 401

        it "POST /projects" $ do
            post "/projects" "" `shouldRespondWith` 401

    describe "GET /projects" $ do
        it "responds with a 200 if signed in" $ do
            (_, session) <- createUser conn
            run (withSession session $ get' "/projects") `shouldRespondWith` 200

    describe "GET /projects/new" $ do
        it "responds with a 200 if signed in" $ do
            (_, session) <- createUser conn
            run (withSession session $ get' "/projects/new")`shouldRespondWith` 200

    describe "GET /projects/:id" $ do
        it "responds with a 200 for a project the user is linked to" $ do
            (userId, session) <- createUser conn
            (P.ProjectId projectId) <- liftIO $ P.create conn "project" "description"
            _ <- liftIO $ P.addUser conn (P.ProjectId projectId) userId
            let url = BS.pack $ "/projects/" <> show projectId
            run (withSession session $ get' url) `shouldRespondWith` 200

        it "responds with a 404 for a project the user is not linked to" $ do
            (_, session) <- createUser conn
            (P.ProjectId projectId) <- liftIO $ P.create conn "project" "description"
            let url = BS.pack $ "/projects/" <> show projectId
            run (withSession session $ get' url) `shouldRespondWith` 404

    describe "GET /projects/:id/edit" $ do
        it "responds with a 200 for a project the user is linked to" $ do
            (userId, session) <- createUser conn
            (P.ProjectId projectId) <- liftIO $ P.create conn "project" "description"
            _ <- liftIO $ P.addUser conn (P.ProjectId projectId) userId
            let url = BS.pack $ "/projects/" <> show projectId <> "/edit"
            run (withSession session $ get' url) `shouldRespondWith` 200

        it "responds with a 404 for a project the user is not linked to" $ do
            (_, session) <- createUser conn
            (P.ProjectId projectId) <- liftIO $ P.create conn "project" "description"
            let url = BS.pack $ "/projects/" <> show projectId <> "/edit"
            run (withSession session $ get' url) `shouldRespondWith` 404

    describe "POST /projects" $ do
        it "creates a project linked to the user and responds with a 302" $ do
            (userId, session) <- createUser conn
            let request = postHtmlForm' "/projects" [("name", "project"), ("description", "the project")]
            run (withSession session request) `shouldRespondWith` 302
            found <- liftIO $ P.findByName conn "project"
            liftIO $ P.name <$> found `shouldBe` Just "project"
            liftIO $ P.description <$> found `shouldBe` Just "the project"
            allForUser <- liftIO $ P.allByUserId conn userId
            let (Just project) = found
            liftIO $ allForUser `shouldBe` [project]

    describe "PUT /projects/:id" $ do
        it "updates a project and responds with a 302 for a project the user is linked to" $ do
            (userId, session) <- createUser conn
            (P.ProjectId projectId) <- liftIO $ P.create conn "name" "desc"
            _ <- liftIO $ P.addUser conn (P.ProjectId projectId) userId
            let url = BS.pack $ "/projects/" <> show projectId
            let request = putHtmlForm' url [("name", "new name"), ("description", "new desc")]

            run (withSession session request) `shouldRespondWith` 302

            found <- liftIO $ P.find conn (P.ProjectId projectId)
            liftIO $ found `shouldBe` (Just $ P.Project (P.ProjectId projectId) "new name" "new desc")
        it "responds with a 404 for a project the user is not linked to" $ do
            (_, session) <- createUser conn
            (P.ProjectId projectId) <- liftIO $ P.create conn "name" "desc"
            let url = BS.pack $ "/projects/" <> show projectId
            let request = putHtmlForm' url [("name", "new name"), ("description", "new desc")]

            run (withSession session request) `shouldRespondWith` 404

            found <- liftIO $ P.find conn (P.ProjectId projectId)
            liftIO $ found `shouldBe` (Just $ P.Project (P.ProjectId projectId) "name" "desc")

    describe "DELETE /projects/:id" $ do
        it "deletes a projects and responds with a 302 for a project the user is linked to" $ do
            (userId, session) <- createUser conn
            (P.ProjectId projectId) <- liftIO $ P.create conn "name" "desc"
            _ <- liftIO $ P.addUser conn (P.ProjectId projectId) userId
            let url = BS.pack $ "/projects/" <> show projectId

            run (withSession session $ delete' url) `shouldRespondWith` 302
            found <- liftIO $ P.find conn (P.ProjectId projectId)
            liftIO $ found `shouldBe` Nothing

        it "responds with a 404 for a project the user is not linked to" $ do
            (_, session) <- createUser conn
            (P.ProjectId projectId) <- liftIO $ P.create conn "name" "desc"
            let url = BS.pack $ "/projects/" <> show projectId

            run (withSession session $ delete' url) `shouldRespondWith` 404
            found <- liftIO $ P.find conn (P.ProjectId projectId)
            liftIO $ found `shouldBe` Just (P.Project (P.ProjectId projectId) "name" "desc")
