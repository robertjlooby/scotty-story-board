{-# LANGUAGE OverloadedStrings #-}

module ProjectsControllerSpec where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Monoid ((<>))
import           Database.PostgreSQL.Simple (Connection)
import           Test.Hspec (Spec, describe, it, shouldBe)
import           Test.Hspec.Wai (WaiSession, delete, get, liftIO, post, put, shouldRespondWith, with)
import qualified Web.Scotty as S

import           Helpers (delete', get', postHtmlForm', putHtmlForm', run, withSession)
import qualified Project as P
import           ProjectsController (app)
import           Session (Session(Session))
import qualified User as U

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
            project <- createLinkedProject conn userId
            run (withSession session . get' $ urlFor project) `shouldRespondWith` 200

        it "responds with a 404 for a project the user is not linked to" $ do
            (_, session) <- createUser conn
            project <- liftIO $ P.create conn "name" "desc"
            run (withSession session . get' $ urlFor project) `shouldRespondWith` 404

    describe "GET /projects/:id/edit" $ do
        it "responds with a 200 for a project the user is linked to" $ do
            (userId, session) <- createUser conn
            project <- createLinkedProject conn userId
            run (withSession session . get' $ urlFor project <> "/edit") `shouldRespondWith` 200

        it "responds with a 404 for a project the user is not linked to" $ do
            (_, session) <- createUser conn
            project <- liftIO $ P.create conn "name" "desc"
            run (withSession session . get' $ urlFor project <> "/edit") `shouldRespondWith` 404

    describe "POST /projects" $ do
        it "creates a project linked to the user and responds with a 302" $ do
            (userId, session) <- createUser conn
            let request = postHtmlForm' "/projects" [("name", "project"), ("description", "the project")]
            run (withSession session request) `shouldRespondWith` 302
            liftIO $ do
                found <- P.findByName conn "project"
                P.name <$> found `shouldBe` Just "project"
                P.description <$> found `shouldBe` Just "the project"
                allForUser <- P.allByUserId conn userId
                let (Just project) = found
                allForUser `shouldBe` [project]

    describe "PUT /projects/:id" $ do
        it "updates a project and responds with a 302 for a project the user is linked to" $ do
            (userId, session) <- createUser conn
            project <- createLinkedProject conn userId

            let request = putHtmlForm' (urlFor project) [("name", "new name"), ("description", "new desc")]
            run (withSession session request) `shouldRespondWith` 302

            liftIO $ do
                found <- P.find conn (P.id_ project)
                found `shouldBe` (Just $ project {P.name = "new name", P.description = "new desc"})

        it "responds with a 404 for a project the user is not linked to" $ do
            (_, session) <- createUser conn
            project <- liftIO $ P.create conn "name" "desc"
            let request = putHtmlForm' (urlFor project) [("name", "new name"), ("description", "new desc")]

            run (withSession session request) `shouldRespondWith` 404

            liftIO $ do
                found <- P.find conn (P.id_ project)
                found `shouldBe` Just project

    describe "DELETE /projects/:id" $ do
        it "deletes a projects and responds with a 302 for a project the user is linked to" $ do
            (userId, session) <- createUser conn
            project <- createLinkedProject conn userId

            run (withSession session . delete' $ urlFor project) `shouldRespondWith` 302
            liftIO $ do
                found <- P.find conn (P.id_ project)
                found `shouldBe` Nothing

        it "responds with a 404 for a project the user is not linked to" $ do
            (_, session) <- createUser conn
            project <- liftIO $ P.create conn "name" "desc"

            run (withSession session . delete' $ urlFor project) `shouldRespondWith` 404
            liftIO $ do
                found <- P.find conn (P.id_ project)
                found `shouldBe` Just project

createUser :: Connection -> WaiSession (U.UserId, Session)
createUser conn = do
    user <- liftIO $ U.create conn "user" "email"
    return (U.id_ user, Session (U.id_ user))

createLinkedProject :: Connection -> U.UserId -> WaiSession P.Project
createLinkedProject conn userId = liftIO $ do
    project <- P.create conn "name" "desc"
    _ <- P.addUser conn (P.id_ project) userId
    return project

urlFor :: P.Project -> ByteString
urlFor project =
    let (P.ProjectId projectId) = P.id_ project
    in
        BS.pack $ "/projects/" <> show projectId
