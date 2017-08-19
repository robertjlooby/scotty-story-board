{-# LANGUAGE OverloadedStrings #-}

module ProjectViews where

import           Data.Monoid ((<>))
import           Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes
import qualified Web.Scotty as S

import qualified Layouts
import qualified Project as P

index :: [P.Project] -> S.ActionM ()
index projects = Layouts.app $ do
    H.div ! class_ "container" $ do
        H.div ! class_ "row" $ do
            H.div ! class_ "six columns" $ do
                ul $ do
                    mapM_ showProject projects
                p . (a ! href "/projects/new") $ "New"
                p . (a ! href "/") $ "Home"
  where
    showProject project = do
        (li . (a ! href (toValue project)) . toHtml . P._projectName) project

show_ :: P.Project -> S.ActionM ()
show_ project = Layouts.app $ do
    H.div ! class_ "container" $ do
        H.div ! class_ "row" $ do
            H.div ! class_ "six columns" $ do
                ul $ do
                    li . toHtml $ "Name: " <> P._projectName project
                    li . toHtml $ "Description: " <> P._projectDescription project
        H.div ! class_ "row" $ do
            H.form ! action (toValue project) ! method "POST" $ do
                input ! type_ "hidden" ! name "_method" ! value "DELETE"
                (a ! class_ "button" ! href (toValue project <> "/edit")) $ "Edit"
                input ! type_ "submit" ! value "Delete"
        H.div ! class_ "row" $ do
            H.div ! class_ "six columns" $ do
                (a ! href "/projects") $ "All"

new :: S.ActionM ()
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
                a ! class_ "button" ! href "/projects" $ "Cancel"
                input ! class_ "button-primary" ! type_ "submit" ! value "Submit"

edit :: P.Project -> S.ActionM ()
edit project = Layouts.app $ do
    H.div ! class_ "container" $ do
        H.form ! action (toValue project) ! method "POST" $ do
            input ! type_ "hidden" ! name "_method" ! value "PUT"
            H.div ! class_ "row" $ do
                H.div ! class_ "six columns" $ do
                    H.label ! for "name" $ toHtml ("Name: " :: String)
                    input ! class_ "u-full-width" ! type_ "text" ! name "name" ! value (toValue . P._projectName $ project)
                H.div ! class_ "six columns" $ do
                    H.label ! for "description" $ toHtml ("Description: " :: String)
                    input ! class_ "u-full-width" ! type_ "text" ! name "description" ! value (toValue . P._projectDescription $ project)
            H.div ! class_ "row" $ do
                a ! class_ "button" ! href (toValue project) $ "Cancel"
                input ! class_ "button-primary" ! type_ "submit" ! value "Submit"
