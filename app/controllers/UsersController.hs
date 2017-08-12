{-# LANGUAGE OverloadedStrings #-}

module UsersController where

import qualified Data.Text.Lazy as T
import           Network.HTTP.Types.Status (notFound404)
import qualified Web.Scotty as S

import           AppContext (HasDbConn(..))
import qualified ErrorViews
import qualified User as U
import qualified UserViews
import           Session (authorized, userId)

app :: HasDbConn a => a -> S.ScottyM ()
app context = do
    let conn = getDbConn context
    S.get "/users/:id/edit" $ authorized $ \session -> do
        id_ <- S.param "id"
        if userId session == (U.UserId id_)
           then do
               (Just user) <- S.liftAndCatchIO $ U.find conn (userId session)
               UserViews.edit user
           else do
               S.status notFound404
               ErrorViews.notFound

    S.put "/users/:id" $ authorized $ \session -> do
        id_ <- S.param "id"
        name <- S.param "name"
        email <- S.param "email"
        if userId session == (U.UserId id_)
           then do
               (Just user) <- S.liftAndCatchIO $ U.find conn (userId session)
               _ <- S.liftAndCatchIO $ U.update conn $ user {U.name = name, U.email = email}
               S.redirect $ T.pack ("/users/" ++ show id_ ++ "/edit")
           else do
               S.status notFound404
               ErrorViews.notFound
