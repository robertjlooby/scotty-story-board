{-# LANGUAGE OverloadedStrings #-}

module ErrorViews (unauthorized) where

import           Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes
import qualified Web.Scotty as S

import qualified Layouts

unauthorized :: S.ActionM ()
unauthorized = Layouts.app $ do
    H.div ! class_ "container" $ do
        H.div ! class_ "row" $ do
            H.div ! class_ "six columns" $ do
                h2 "Unauthorized"
                p . (a ! href "/login") $ "Please login"