{-# LANGUAGE OverloadedStrings #-}

module Main where

import App (app)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import System.Environment (getEnv)
import qualified Web.Scotty as S

main :: IO ()
main = do
    port <- read <$> getEnv "PORT"
    S.scotty port $ do
        S.middleware logStdout
        S.middleware $ staticPolicy (addBase "app/static")
        app
