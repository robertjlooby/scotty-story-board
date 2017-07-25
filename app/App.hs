{-# LANGUAGE OverloadedStrings #-}

module App where

import qualified Data.Text.Lazy as T
import           Database.PostgreSQL.Simple (Connection)
import           Network.HTTP.Types.Status (notFound404)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Web.Scotty as S

import qualified Project as P
import qualified ProjectViews

app :: Connection -> S.ScottyM ()
app conn = do
    S.get "/projects" $ do
        projects <- S.liftAndCatchIO $ P.findAll conn
        S.html $ renderHtml $ ProjectViews.index projects

    S.get "/projects/:id" $ do
        id_ <- S.param "id"
        project <- S.liftAndCatchIO $ P.find conn (P.ProjectId id_)
        case project of
            Just p -> S.html $ renderHtml $ ProjectViews.show_ p
            Nothing -> S.status notFound404

    S.get "/projects/:id/edit" $ do
        id_ <- S.param "id"
        project <- S.liftAndCatchIO $ P.find conn (P.ProjectId id_)
        case project of
            Just p -> S.html $ renderHtml $ ProjectViews.edit p
            Nothing -> S.status notFound404

    S.put "/projects/:id" $ do
        id_ <- S.param "id"
        name <- S.param "name"
        description <- S.param "description"
        _ <- S.liftAndCatchIO $ P.update conn $ P.Project (P.ProjectId id_) name description
        S.redirect $ T.pack ("/projects/" ++ show id_)

    S.delete "/projects/:id" $ do
        id_ <- S.param "id"
        _ <- S.liftAndCatchIO $ P.delete conn (P.ProjectId id_)
        S.redirect "/projects"

    S.get "/projects/new" $ do
        S.html $ renderHtml $ ProjectViews.new

    S.post "/projects" $ do
        name <- S.param "name"
        description <- S.param "description"
        _ <- S.liftAndCatchIO $ P.create conn name description
        S.redirect "/projects"
