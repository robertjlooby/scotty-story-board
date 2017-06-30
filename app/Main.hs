{-# LANGUAGE OverloadedStrings #-}

module Main where

import App (app)
import qualified Web.Scotty as S

main :: IO ()
main = do
  S.scotty 3000 app
