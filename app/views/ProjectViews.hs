{-# LANGUAGE OverloadedStrings #-}

module ProjectViews where

import Data.Monoid ((<>))
import qualified Layouts
import qualified Project as P
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes

index :: [P.Project] -> Html
index projects = Layouts.app $ do
    ul $ do
        mapM_ showProject projects
        li . (a ! href "/projects/new") $ "New"
  where
    showProject project = do
        let (P.ProjectId id_) = P.id_ project
        (li . (a ! href ("/projects/" <> toValue id_)) . toHtml . P.name) project

show_ :: P.Project -> Html
show_ project = Layouts.app $ do
    ul $ do
        let (P.ProjectId id_) = P.id_ project
        li . toHtml $ "Name: " <> P.name project
        li . toHtml $ "Description: " <> P.description project
        li . (a ! href ("/projects/" <> toValue id_ <> "/edit")) $ "Edit"
        li $ do
            H.form ! action ("/projects/" <> toValue id_) ! method "POST" $ do
                input ! type_ "hidden" ! name "_method" ! value "DELETE"
                input ! type_ "submit" ! value "Delete"
        li . (a ! href "/projects") $ "All"

new :: Html
new = Layouts.app $ do
    H.form ! action "/projects" ! method "POST" $ do
        H.label $ toHtml ("Name: " :: String)
        input ! type_ "text" ! name "name"
        H.label $ toHtml ("Description: " :: String)
        input ! type_ "text" ! name "description"
        input ! type_ "submit"

edit :: P.Project -> Html
edit project = Layouts.app $ do
    let (P.ProjectId id_) = P.id_ project
    H.form ! action ("/projects/" <> toValue id_) ! method "POST" $ do
        input ! type_ "hidden" ! name "_method" ! value "PUT"
        H.label $ toHtml ("Name: " :: String)
        input ! type_ "text" ! name "name" ! value (toValue . P.name $ project)
        H.label $ toHtml ("Description: " :: String)
        input ! type_ "text" ! name "description" ! value (toValue . P.description $ project)
        input ! type_ "submit"
