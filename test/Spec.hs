{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as BS8
import           Database.PostgreSQL.Simple (connectPostgreSQL)
import           Database.PostgreSQL.Simple.Util (withTransactionRolledBack)
import           Test.Hspec (around_, hspec)
import           System.Environment (getEnv)

import qualified IndexControllerSpec
import qualified OAuthLoginSpec
import qualified ProjectSpec
import qualified ProjectsControllerSpec
import qualified UserSpec

main :: IO ()
main = do
    conn <- BS8.pack <$> getEnv "DATABASE_URL" >>= connectPostgreSQL
    hspec $ around_ (withTransactionRolledBack conn) $ do
        IndexControllerSpec.spec
        OAuthLoginSpec.spec conn
        ProjectSpec.spec conn
        ProjectsControllerSpec.spec conn
        UserSpec.spec conn
