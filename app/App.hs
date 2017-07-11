{-# LANGUAGE OverloadedStrings #-}

module App where

import Database.PostgreSQL.Simple (Connection)
import qualified IndexViews
import qualified Project as P
import qualified ProjectViews
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Web.Scotty as S

app :: Connection -> S.ScottyM ()
app conn = do
    S.get "/" $ do
        S.html $ renderHtml IndexViews.index
    S.get "/projects" $ do
        projects <- S.liftAndCatchIO $ P.findAll conn
        S.html $ renderHtml $ ProjectViews.index projects
    S.get "/projects/new" $ do
        S.html $ renderHtml $ ProjectViews.new
    S.post "/projects" $ do
        name <- S.param "name"
        description <- S.param "description"
        _ <- S.liftAndCatchIO $ P.create conn name description
        S.redirect "/"
