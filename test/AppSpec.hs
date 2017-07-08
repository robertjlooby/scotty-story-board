{-# LANGUAGE OverloadedStrings #-}

module AppSpec (spec) where

import App (app)
import Database.PostgreSQL.Simple (Connection)
import qualified Models.Project as P
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Wai (get, liftIO, postHtmlForm, shouldRespondWith, with)
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
    describe "POST /projects" $ do
        it "responds with a 302" $ do
            postHtmlForm "/projects" [("name", "project"), ("description", "the project")] `shouldRespondWith` 302
        it "creates a project" $ do
            _ <- postHtmlForm "/projects" [("name", "project"), ("description", "the project")]
            found <- liftIO $ P.findByName conn "project"
            liftIO $ found `shouldBe` Just (P.Project "project" "the project")
