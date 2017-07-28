{-# LANGUAGE OverloadedStrings #-}

module IndexViews (index) where

import           Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes

import qualified Layouts
import           Session (Session)

index :: Maybe Session -> Html
index session = Layouts.app $ do
    H.div ! class_ "container" $ do
        H.div ! class_ "row" $ do
            H.div ! class_ "six columns" $ do
                h2 "Hello!"
                loginLogoutLink session
                p . (a ! href "/projects") $ "All Projects"
  where
    loginLogoutLink Nothing = p . (a ! href "/login") $ "Login"
    loginLogoutLink (Just _) = p . (a ! href "/logout") $ "Logout"
