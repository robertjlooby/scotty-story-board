{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified App
import AppContext (getContext, environment, port)
import qualified Auth
import qualified Data.ByteString.Char8 as BS8
import Database.PostgreSQL.Simple (connectPostgreSQL)
import qualified Index
import Network.HTTP.Conduit (newManager, tlsManagerSettings)
import Network.Wai.Middleware.ForceSSL (forceSSL)
import Network.Wai.Middleware.MethodOverridePost (methodOverridePost)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import System.Environment (getEnv)
import qualified Web.Scotty as S

sslMiddleware :: String -> S.ScottyM ()
sslMiddleware "production" = S.middleware forceSSL
sslMiddleware _ = return ()

main :: IO ()
main = do
    appContext <- getContext
    conn <- BS8.pack <$> getEnv "DATABASE_URL" >>= connectPostgreSQL
    mgr <- newManager tlsManagerSettings
    S.scotty (port appContext) $ do
        S.middleware methodOverridePost
        S.middleware logStdout
        sslMiddleware (environment appContext)
        S.middleware $ staticPolicy (addBase "app/static")
        Index.app
        Auth.app conn mgr appContext
        App.app conn
