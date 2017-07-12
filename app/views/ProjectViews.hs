{-# LANGUAGE OverloadedStrings #-}

module ProjectViews where

import Data.Monoid ((<>))
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
      ul $ mapM_ showProject projects
  where
    showProject project = do
      let (P.ProjectId id_) = P.id_ project
      (li . (a ! href ("/projects/" <> (stringValue (show id_)))) . toHtml . P.name) project

show_ :: P.Project -> Html
show_ project = docTypeHtml $ do
    H.head $ do
        link ! rel "stylesheet" ! href "normalize.css"
        link ! rel "stylesheet" ! href "skeleton.css"
        H.title "Scotty Story Board"
    body $ do
      ul $ do
        li . toHtml $ "Name: " <> P.name project
        li . toHtml $ "Description: " <> P.description project
        li . (a ! href "/projects") $ "All"

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
