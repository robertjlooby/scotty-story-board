{-# LANGUAGE OverloadedStrings #-}

module IndexViews (index) where

import           Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes

import qualified Layouts

index :: Html
index = Layouts.app $ do
    H.div ! class_ "container" $ do
        H.div ! class_ "row" $ do
            H.div ! class_ "six columns" $ do
                h2 "Hello!"
                p . (a ! href "/login") $ "Login"
                p . (a ! href "/projects") $ "All Projects"
