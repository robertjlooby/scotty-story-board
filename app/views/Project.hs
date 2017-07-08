{-# LANGUAGE OverloadedStrings #-}

module Views.Project (index) where

import qualified Project as P
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes

index :: [P.Project] -> Html
index projects = docTypeHtml $ do
    H.head $ do
        link ! rel "stylesheet" ! href "normalize.css"
        link ! rel "stylesheet" ! href "skeleton.css"
        H.title "Scotty Story Board"
    body $ do
      ul $ mapM_ (li . toHtml . P.name) projects
