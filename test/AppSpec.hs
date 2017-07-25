{-# LANGUAGE OverloadedStrings #-}

module AppSpec where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Monoid ((<>))
import           Database.PostgreSQL.Simple (Connection)
import           Test.Hspec (Spec, describe, it, shouldBe)
import           Test.Hspec.Wai (delete, get, liftIO, postHtmlForm, request, shouldRespondWith, with)
import qualified Web.Scotty as S

import           App (app)
import qualified Project as P

spec :: Connection -> Spec
spec conn = with (S.scottyApp $ app conn) $ do
    describe "GET /projects" $ do
        it "responds with a 200" $ do
            get "/projects" `shouldRespondWith` 200

    describe "GET /projects/new" $ do
        it "responds with a 200" $ do
            get "/projects/new" `shouldRespondWith` 200

    describe "GET /projects/:id" $ do
        it "responds with a 200 for a project" $ do
            (P.ProjectId projectId) <- liftIO $ P.create conn "project" "description"
            let url = BS.pack $ "/projects/" <> show projectId
            get url `shouldRespondWith` 200
        it "responds with a 404 for project not found" $ do
            get "/projects/0" `shouldRespondWith` 404

    describe "GET /projects/:id/edit" $ do
        it "responds with a 200 for a project" $ do
            (P.ProjectId projectId) <- liftIO $ P.create conn "project" "description"
            let url = BS.pack $ "/projects/" <> show projectId <> "/edit"
            get url `shouldRespondWith` 200
        it "responds with a 404 for project not found" $ do
            get "/projects/0/edit" `shouldRespondWith` 404

    describe "POST /projects" $ do
        it "responds with a 302" $ do
            postHtmlForm "/projects" [("name", "project"), ("description", "the project")] `shouldRespondWith` 302
        it "creates a project" $ do
            _ <- postHtmlForm "/projects" [("name", "project"), ("description", "the project")]
            found <- liftIO $ P.findByName conn "project"
            liftIO $ P.name <$> found `shouldBe` Just "project"
            liftIO $ P.description <$> found `shouldBe` Just "the project"

    describe "PUT /projects/:id" $ do
        it "updates a project and responds with a 302" $ do
            (P.ProjectId projectId) <- liftIO $ P.create conn "name" "desc"
            let url = BS.pack $ "/projects/" <> show projectId
            let body = BSL.pack $ "name=new+name&description=new+desc"

            request "PUT" url [("Content-Type", "application/x-www-form-urlencoded")] body `shouldRespondWith` 302

            found <- liftIO $ P.find conn (P.ProjectId projectId)
            liftIO $ found `shouldBe` (Just $ P.Project (P.ProjectId projectId) "new name" "new desc")

    describe "DELETE /projects/:id" $ do
        it "responds with a 302" $ do
            delete "/projects/0" `shouldRespondWith` 302
        it "deletes a project" $ do
            (P.ProjectId projectId) <- liftIO $ P.create conn "project" "description"
            let url = BS.pack $ "/projects/" <> show projectId

            _ <- delete url

            found <- liftIO $ P.find conn (P.ProjectId projectId)
            liftIO $ found `shouldBe` Nothing
