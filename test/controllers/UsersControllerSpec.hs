{-# LANGUAGE OverloadedStrings #-}

module UsersControllerSpec where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Monoid ((<>))
import           Database.PostgreSQL.Simple (Connection)
import           Test.Hspec (Spec, describe, it, shouldBe)
import           Test.Hspec.Wai (WaiSession, get, liftIO, put, shouldRespondWith, with)
import qualified Web.Scotty as S

import           AppContext (HasDbConn(..))
import           Helpers (get', putHtmlForm', run, withSession)
import           Session (Session(..))
import qualified User as U
import           UsersController (app)

spec :: HasDbConn a => a -> Spec
spec context = with (S.scottyApp $ app context) $ do
    let conn = getDbConn context
    describe "all uesrs routes respond with a 401 if not logged in" $ do
        it "GET /users/:id/edit" $ do
            get "/users/0/edit" `shouldRespondWith` 401

        it "PUT /users/:id" $ do
            put "/users/0" "" `shouldRespondWith` 401

    describe "GET /users/:id/edit" $ do
        it "responds with a 200 for the current user" $ do
            (user, session) <- createUser conn
            run (withSession session . get' $ urlFor user <> "/edit") `shouldRespondWith` 200

        it "responds with a 404 any other user" $ do
            (_, session) <- createUser conn
            otherUser <- liftIO $ U.create conn "other" "email2"
            run (withSession session . get' $ urlFor otherUser <> "/edit") `shouldRespondWith` 404

    describe "PUT /users/:id" $ do
        it "updates a user and responds with a 302 for the current user" $ do
            (user, session) <- createUser conn

            let request = putHtmlForm' (urlFor user) [("name", "new name"), ("email", "new email")]
            run (withSession session request) `shouldRespondWith` 302

            liftIO $ do
                found <- U.runUserFindQuery conn $ U.findQuery (U.id_ user)
                found `shouldBe` (Just $ user {U.name = "new name", U.email = "new email"})

        it "responds with a 404 any other user" $ do
            (user, session) <- createUser conn
            otherUser <- liftIO $ U.create conn "other" "email2"

            let request = putHtmlForm' (urlFor otherUser) [("name", "new name"), ("email", "new email")]
            run (withSession session request) `shouldRespondWith` 404

            liftIO $ do
                found <- U.runUserFindQuery conn $ U.findQuery (U.id_ user)
                found `shouldBe` Just user
                other <- U.runUserFindQuery conn $ U.findQuery (U.id_ otherUser)
                other `shouldBe` Just otherUser

createUser :: Connection -> WaiSession (U.User, Session)
createUser conn = do
    user <- liftIO $ U.create conn "user" "email"
    return (user, Session (U.id_ user))

urlFor :: U.User -> ByteString
urlFor user =
    let (U.UserId userId') = U.id_ user
    in
        BS.pack $ "/users/" <> show userId'
