{-# LANGUAGE OverloadedStrings #-}

module ProjectsController where

import           Control.Lens ((^.))
import qualified Data.Text.Lazy as T
import qualified Web.Scotty as S

import           AppContext (HasDbConn(..))
import           Project (projectId)
import qualified Project as P
import qualified ProjectViews
import           Session (authorized, sessionUserId, with404)

app :: HasDbConn a => a -> S.ScottyM ()
app context = do
    let conn = getDbConn context
    S.get "/projects" $ authorized $ \session -> do
        projects <- S.liftAndCatchIO $ P.runProjectQuery conn $ P.allByUserIdQuery (session^.sessionUserId)
        ProjectViews.index projects

    S.get "/projects/:id" $ authorized $ \session -> do
        id_ <- S.param "id"
        project <- S.liftAndCatchIO $ P.runProjectFindQuery conn $ P.findByUserIdQuery (session^.sessionUserId) (P.ProjectId id_)
        with404 project ProjectViews.show_

    S.get "/projects/:id/edit" $ authorized $ \session -> do
        id_ <- S.param "id"
        project <- S.liftAndCatchIO $ P.runProjectFindQuery conn $ P.findByUserIdQuery (session^.sessionUserId) (P.ProjectId id_)
        with404 project ProjectViews.edit

    S.put "/projects/:id" $ authorized $ \session -> do
        id_ <- S.param "id"
        name <- S.param "name"
        description <- S.param "description"
        project <- S.liftAndCatchIO $ P.runProjectFindQuery conn $ P.findByUserIdQuery (session^.sessionUserId) (P.ProjectId id_)
        with404 project $ \p -> do
            _ <- S.liftAndCatchIO $ P.update conn $ p {P._projectName = name, P._projectDescription = description}
            S.redirect $ T.pack ("/projects/" ++ show id_)

    S.delete "/projects/:id" $ authorized $ \session -> do
        id_ <- S.param "id"
        project <- S.liftAndCatchIO $ P.runProjectFindQuery conn $ P.findByUserIdQuery (session^.sessionUserId) (P.ProjectId id_)
        with404 project $ \_ -> do
            _ <- S.liftAndCatchIO $ P.delete conn (P.ProjectId id_)
            S.redirect "/projects"

    S.get "/projects/new" $ authorized $ \_ -> do
        ProjectViews.new

    S.post "/projects" $ authorized $ \session -> do
        name <- S.param "name"
        description <- S.param "description"
        project <- S.liftAndCatchIO $ P.create conn name description
        _ <- S.liftAndCatchIO $ P.addUser conn (project^.projectId) (session^.sessionUserId)
        S.redirect "/projects"
