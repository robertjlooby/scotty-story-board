{-# LANGUAGE OverloadedStrings #-}

module AuthViews (login) where

import           Data.ByteString (ByteString)
import           Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes
import qualified Web.Scotty as S

import qualified Layouts

login :: ByteString -> S.ActionM ()
login googleLoginUrl = Layouts.app $ do
    H.div ! class_ "container" $ do
        H.div ! class_ "row" $ do
            H.div ! class_ "six columns" $ do
                h2 "Login"
                p . (a ! (href . unsafeByteStringValue $ googleLoginUrl)) $ "Login with Google"
