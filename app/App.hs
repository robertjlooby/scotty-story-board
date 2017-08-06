{-# LANGUAGE OverloadedStrings #-}

module App where

import qualified Data.Text.Lazy as T
import           Database.PostgreSQL.Simple (Connection)
import           Network.HTTP.Types.Status (notFound404)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Web.Scotty as S

import qualified Project as P
import qualified ProjectViews
import           Session (authorized, userId)

app :: Connection -> S.ScottyM ()
app conn = do
    S.get "/projects" $ authorized $ \session -> do
        projects <- S.liftAndCatchIO $ P.allByUserId conn (userId session)
        S.html $ renderHtml $ ProjectViews.index projects

    S.get "/projects/:id" $ authorized $ \session -> do
        id_ <- S.param "id"
        project <- S.liftAndCatchIO $ P.findByUserId conn (userId session) (P.ProjectId id_)
        case project of
            Just p -> S.html $ renderHtml $ ProjectViews.show_ p
            Nothing -> S.status notFound404

    S.get "/projects/:id/edit" $ authorized $ \session -> do
        id_ <- S.param "id"
        project <- S.liftAndCatchIO $ P.findByUserId conn (userId session) (P.ProjectId id_)
        case project of
            Just p -> S.html $ renderHtml $ ProjectViews.edit p
            Nothing -> S.status notFound404

    S.put "/projects/:id" $ authorized $ \session -> do
        id_ <- S.param "id"
        name <- S.param "name"
        description <- S.param "description"
        project <- S.liftAndCatchIO $ P.findByUserId conn (userId session) (P.ProjectId id_)
        case project of
            Just _ -> do
                _ <- S.liftAndCatchIO $ P.update conn $ P.Project (P.ProjectId id_) name description
                S.redirect $ T.pack ("/projects/" ++ show id_)
            Nothing -> S.status notFound404

    S.delete "/projects/:id" $ authorized $ \session -> do
        id_ <- S.param "id"
        project <- S.liftAndCatchIO $ P.findByUserId conn (userId session) (P.ProjectId id_)
        case project of
            Just _ -> do
                _ <- S.liftAndCatchIO $ P.delete conn (P.ProjectId id_)
                S.redirect "/projects"
            Nothing -> S.status notFound404

    S.get "/projects/new" $ authorized $ \_ -> do
        S.html $ renderHtml $ ProjectViews.new

    S.post "/projects" $ authorized $ \session -> do
        name <- S.param "name"
        description <- S.param "description"
        project <- S.liftAndCatchIO $ P.create conn name description
        _ <- S.liftAndCatchIO $ P.addUser conn (P.id_ project) (userId session)
        S.redirect "/projects"
