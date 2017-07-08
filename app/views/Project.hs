{-# LANGUAGE OverloadedStrings #-}

module Views.Project (index, new) where

import qualified Models.Project as P
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

new :: Html
new = docTypeHtml $ do
    H.head $ do
        link ! rel "stylesheet" ! href "normalize.css"
        link ! rel "stylesheet" ! href "skeleton.css"
        H.title "Scotty Story Board"
    body $ do
      H.form ! action "/projects" ! method "POST" $ do
        H.label $ toHtml ("Name: " :: String)
        input ! type_ "text" ! name "name"
        H.label $ toHtml ("Description: " :: String)
        input ! type_ "text" ! name "description"
        input ! type_ "submit"
