{-# LANGUAGE OverloadedStrings #-}

module IndexViews (index) where

import qualified Layouts
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes

index :: Html
index = Layouts.app $ do
    p "Hello!"
    p . (a ! href "/projects") $ "All Projects"
