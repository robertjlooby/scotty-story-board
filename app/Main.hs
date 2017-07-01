{-# LANGUAGE OverloadedStrings #-}

module Main where

import App (app)
import System.Environment (getEnv)
import qualified Web.Scotty as S

main :: IO ()
main = do
  port <- read <$> getEnv "PORT"
  S.scotty port app
