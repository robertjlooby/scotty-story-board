{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as BS8
import Database.PostgreSQL.Simple (connectPostgreSQL)
import Database.PostgreSQL.Simple.Util (withTransactionRolledBack)
import Test.Hspec (around_, hspec)
import System.Environment (getEnv)

import qualified AppSpec
import qualified ProjectSpec

main :: IO ()
main = do
    conn <- BS8.pack <$> getEnv "DATABASE_URL" >>= connectPostgreSQL
    hspec $ around_ (withTransactionRolledBack conn) $ do
        AppSpec.spec conn
        ProjectSpec.spec conn
