{-# LANGUAGE OverloadedStrings #-}

module ProjectViews where

import           Data.Monoid ((<>))
import           Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes

import qualified Layouts
import qualified Project as P

index :: [P.Project] -> Html
index projects = Layouts.app $ do
    H.div ! class_ "container" $ do
        H.div ! class_ "row" $ do
            H.div ! class_ "six columns" $ do
                ul $ do
                    mapM_ showProject projects
                p . (a ! href "/projects/new") $ "New"
  where
    showProject project = do
        let (P.ProjectId id_) = P.id_ project
        (li . (a ! href ("/projects/" <> toValue id_)) . toHtml . P.name) project

show_ :: P.Project -> Html
show_ project = Layouts.app $ do
    let (P.ProjectId id_) = P.id_ project
    H.div ! class_ "container" $ do
        H.div ! class_ "row" $ do
            H.div ! class_ "six columns" $ do
                ul $ do
                    li . toHtml $ "Name: " <> P.name project
                    li . toHtml $ "Description: " <> P.description project
        H.div ! class_ "row" $ do
            H.form ! action ("/projects/" <> toValue id_) ! method "POST" $ do
                input ! type_ "hidden" ! name "_method" ! value "DELETE"
                (a ! class_ "button" ! href ("/projects/" <> toValue id_ <> "/edit")) $ "Edit"
                input ! type_ "submit" ! value "Delete"
        H.div ! class_ "row" $ do
            H.div ! class_ "six columns" $ do
                (a ! href "/projects") $ "All"

new :: Html
new = Layouts.app $ do
    H.div ! class_ "container" $ do
        H.form ! action "/projects" ! method "POST" $ do
            H.div ! class_ "row" $ do
                H.div ! class_ "six columns" $ do
                    H.label ! for "name" $ toHtml ("Name: " :: String)
                    input ! class_ "u-full-width" ! type_ "text" ! name "name"
                H.div ! class_ "six columns" $ do
                    H.label ! for "description" $ toHtml ("Description: " :: String)
                    input ! class_ "u-full-width" ! type_ "text" ! name "description"
            H.div ! class_ "row" $ do
                a ! class_ "button"! href "/projects" $ "Cancel"
                input ! class_ "button-primary" ! type_ "submit" ! value "Submit"

edit :: P.Project -> Html
edit project = Layouts.app $ do
    let (P.ProjectId id_) = P.id_ project
    let url = "/projects/" <> toValue id_
    H.div ! class_ "container" $ do
        H.form ! action url ! method "POST" $ do
            input ! type_ "hidden" ! name "_method" ! value "PUT"
            H.div ! class_ "row" $ do
                H.div ! class_ "six columns" $ do
                    H.label ! for "name" $ toHtml ("Name: " :: String)
                    input ! class_ "u-full-width" ! type_ "text" ! name "name" ! value (toValue . P.name $ project)
                H.div ! class_ "six columns" $ do
                    H.label ! for "description" $ toHtml ("Description: " :: String)
                    input ! class_ "u-full-width" ! type_ "text" ! name "description" ! value (toValue . P.description $ project)
            H.div ! class_ "row" $ do
                a ! class_ "button"! href url $ "Cancel"
                input ! class_ "button-primary" ! type_ "submit" ! value "Submit"
