{-# LANGUAGE OverloadedStrings #-}

module AppSpec (spec) where

import App (app)
import qualified Data.ByteString.Char8 as BS
import Data.Monoid ((<>))
import Database.PostgreSQL.Simple (Connection)
import qualified Project as P
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Wai (delete, get, liftIO, postHtmlForm, shouldRespondWith, with)
import qualified Web.Scotty as S

spec :: Connection -> Spec
spec conn = with (S.scottyApp $ app conn) $ do
    describe "GET /" $ do
        it "responds with 200" $ do
            get "/" `shouldRespondWith` 200
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
    describe "POST /projects" $ do
        it "responds with a 302" $ do
            postHtmlForm "/projects" [("name", "project"), ("description", "the project")] `shouldRespondWith` 302
        it "creates a project" $ do
            _ <- postHtmlForm "/projects" [("name", "project"), ("description", "the project")]
            found <- liftIO $ P.findByName conn "project"
            liftIO $ P.name <$> found `shouldBe` Just "project"
            liftIO $ P.description <$> found `shouldBe` Just "the project"
    describe "DELETE /projects/:id" $ do
        it "responds with a 302" $ do
            delete "/projects/0" `shouldRespondWith` 302
        it "deletes a project" $ do
            (P.ProjectId projectId) <- liftIO $ P.create conn "project" "description"
            let url = BS.pack $ "/projects/" <> show projectId

            _ <- delete url

            found <- liftIO $ P.find conn (P.ProjectId projectId)
            liftIO $ found `shouldBe` Nothing
