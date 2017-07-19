{-# LANGUAGE OverloadedStrings #-}

module AuthViews (login) where

import Data.ByteString (ByteString)
import qualified Layouts
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes

login :: ByteString -> Html
login googleLoginUrl = Layouts.app $ do
    H.div ! class_ "container" $ do
        H.div ! class_ "row" $ do
            H.div ! class_ "six columns" $ do
                h2 "Login"
                p . (a ! (href . unsafeByteStringValue $ googleLoginUrl)) $ "Login with Google"
