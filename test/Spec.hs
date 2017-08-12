{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Database.PostgreSQL.Simple.Util (withTransactionRolledBack)
import           Test.Hspec (around_, hspec)

import           AppContext (getContext, getDbConn)
import qualified IndexControllerSpec
import qualified OAuthLoginSpec
import qualified ProjectSpec
import qualified ProjectsControllerSpec
import qualified SessionSpec
import qualified UserSpec
import qualified UsersControllerSpec

main :: IO ()
main = do
    appContext <- getContext "test"
    let conn = getDbConn appContext
    hspec $ around_ (withTransactionRolledBack conn) $ do
        IndexControllerSpec.spec
        OAuthLoginSpec.spec appContext
        ProjectSpec.spec appContext
        ProjectsControllerSpec.spec appContext
        SessionSpec.spec
        UserSpec.spec appContext
        UsersControllerSpec.spec appContext
