{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Logging (withStdoutLogging)
import           Network.HTTP.Conduit (newManager, tlsManagerSettings)
import           Network.Wai.Middleware.ForceSSL (forceSSL)
import           Network.Wai.Middleware.MethodOverridePost (methodOverridePost)
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Network.Wai.Middleware.Static (addBase, staticPolicy)
import qualified Web.Scotty as S

import           AppContext (getContext, getEnvironment, getPort)
import qualified AuthorizationController
import qualified ErrorViews
import qualified IndexController
import qualified ProjectsController
import           Session (sessionMiddleware)
import qualified UsersController
import           Util (logError)

sslMiddleware :: String -> S.ScottyM ()
sslMiddleware "production" = S.middleware forceSSL
sslMiddleware _ = return ()

main :: IO ()
main = withStdoutLogging $ do
    appContext <- getContext "development"
    mgr <- newManager tlsManagerSettings
    S.scotty (getPort appContext) $ do
        S.middleware methodOverridePost
        S.middleware logStdout
        sslMiddleware (getEnvironment appContext)
        S.middleware $ staticPolicy (addBase "app/static")
        S.middleware sessionMiddleware
        S.defaultHandler $ \errorMessage -> do
            logError errorMessage
            ErrorViews.serverError

        IndexController.app
        AuthorizationController.app mgr appContext
        ProjectsController.app appContext
        UsersController.app appContext

        S.notFound ErrorViews.notFound
