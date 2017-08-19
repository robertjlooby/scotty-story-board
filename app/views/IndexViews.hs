{-# LANGUAGE OverloadedStrings #-}

module IndexViews (index) where

import           Data.Monoid ((<>))
import           Text.Blaze (toValue)
import           Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes
import qualified Web.Scotty as S

import qualified Layouts
import           Session (Session, _sessionUserId)

index :: Maybe Session -> S.ActionM ()
index session = Layouts.app $ do
    H.div ! class_ "container" $ do
        H.div ! class_ "row" $ do
            H.div ! class_ "six columns" $ do
                h2 "Hello!"
                links session
  where
    links Nothing = p . (a ! href "/login") $ "Login"
    links (Just session') = do
        p . (a ! href "/logout") $ "Logout"
        p . (a ! href "/projects") $ "All Projects"
        p . (a ! href ((toValue (_sessionUserId session')) <> "/edit")) $ "Edit user"
