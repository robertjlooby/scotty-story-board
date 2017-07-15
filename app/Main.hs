{-# LANGUAGE OverloadedStrings #-}

module Main where

import App (app)
import qualified Data.ByteString.Char8 as BS8
import Data.HashMap.Lazy (fromList)
import Database.PostgreSQL.Simple (connectPostgreSQL)
import Network.Wai.Middleware.Auth
import Network.Wai.Middleware.Auth.OAuth2.Github
import Network.Wai.Middleware.Auth.OAuth2.Google
import Network.Wai.Middleware.Auth.Provider
import Network.Wai.Middleware.ForceSSL (forceSSL)
import Network.Wai.Middleware.MethodOverridePost (methodOverridePost)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import System.Environment (getEnv, lookupEnv)
import qualified Web.Scotty as S

getSSLMiddleware :: IO (S.ScottyM ())
getSSLMiddleware = do
    environment <- lookupEnv "APP_ENV"
    if environment == Just "production"
        then return $ S.middleware forceSSL
        else return $ return ()

main :: IO ()
main = do
    conn <- BS8.pack <$> getEnv "DATABASE_URL" >>= connectPostgreSQL
    port <- read <$> getEnv "PORT"
    sslMiddleware <- getSSLMiddleware
    let github = mkGithubProvider "Scotty Story Board" "id" "secret" [] Nothing
    let google = mkGoogleProvider "id" "secret" [] Nothing
    authMiddleware <- mkAuthMiddleware (setAuthProviders (fromList [("Google", Provider google), ("GitHub", Provider github)]) defaultAuthSettings)
    S.scotty port $ do
        S.middleware methodOverridePost
        S.middleware logStdout
        sslMiddleware
        S.middleware $ staticPolicy (addBase "app/static")
        S.middleware authMiddleware
        app conn
