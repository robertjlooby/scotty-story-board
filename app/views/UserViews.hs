{-# LANGUAGE OverloadedStrings #-}

module UserViews where

import           Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes
import qualified Web.Scotty as S

import qualified Layouts
import qualified User as U

edit :: U.User -> S.ActionM ()
edit user = Layouts.app $ do
    H.div ! class_ "container" $ do
        H.form ! action (toValue user) ! method "POST" $ do
            input ! type_ "hidden" ! name "_method" ! value "PUT"
            H.div ! class_ "row" $ do
                H.div ! class_ "six columns" $ do
                    H.label ! for "name" $ toHtml ("Name: " :: String)
                    input ! class_ "u-full-width" ! type_ "text" ! name "name" ! value (toValue . U._userName $ user)
                H.div ! class_ "six columns" $ do
                    H.label ! for "email" $ toHtml ("Email: " :: String)
                    input ! class_ "u-full-width" ! type_ "text" ! name "email" ! value (toValue . U._userEmail $ user)
            H.div ! class_ "row" $ do
                a ! class_ "button" ! href "/" $ "Cancel"
                input ! class_ "button-primary" ! type_ "submit" ! value "Submit"
