{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Session where

import Data.Aeson (FromJSON, ToJSON, encode, defaultOptions, genericToEncoding, toEncoding)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.String (IsString)
import qualified Data.Text.Encoding as E
import GHC.Generics (Generic)
import qualified User
import User (User, UserId)
import Web.ClientSession (Key, decrypt, encryptIO)
import Web.Cookie (def, setCookieName, setCookiePath, setCookieValue)
import qualified Web.Scotty as S
import Web.Scotty.Cookie (getCookie, setCookie)

data Session = Session
    { userId :: UserId
    } deriving (Eq, Generic, Show)

instance ToJSON Session where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Session

sessionCookieName :: IsString a => a
sessionCookieName = "session"

setSessionCookie :: User -> Key -> S.ActionM ()
setSessionCookie user key = do
    let session = Session (User.id_ user)
    encrypted <- S.liftAndCatchIO $ encryptIO key (BSL.toStrict . encode $ session)
    let cookie = def { setCookieName = sessionCookieName
                     , setCookieValue = encrypted
                     , setCookiePath = Just "/"
                     }
    setCookie cookie

getSessionCookie :: Key -> S.ActionM (Maybe ByteString)
getSessionCookie key = do
    cookie <- getCookie sessionCookieName
    case cookie of
      Just d -> return $ decrypt key (E.encodeUtf8 d)
      Nothing -> return Nothing
