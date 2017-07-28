{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Session
    ( Session(..)
    , getSession
    , deleteSession
    , sessionMiddleware
    , setSession
    ) where

import           Data.Aeson (FromJSON, ToJSON, encode, decodeStrict, defaultOptions, genericToEncoding, toEncoding)
import qualified Data.ByteString.Lazy as BSL
import           Data.String (IsString)
import qualified Data.Vault.Lazy as Vault
import           GHC.Generics (Generic)
import           Network.HTTP.Types.Header (hCookie)
import           Network.Wai (Middleware, requestHeaders, vault)
import           System.IO.Unsafe
import           Web.ClientSession (decrypt, encryptIO)
import qualified Web.ClientSession as CS
import           Web.Cookie (def, parseCookies, setCookieName, setCookiePath, setCookieValue)
import qualified Web.Scotty as S
import           Web.Scotty.Cookie (deleteCookie, setCookie)

import           User (UserId)

data Session = Session
    { userId :: UserId
    } deriving (Eq, Generic, Show)

instance ToJSON Session where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Session

sessionCookieName :: IsString a => a
sessionCookieName = "session"

setSession :: Session -> S.ActionM ()
setSession session = do
    encrypted <- S.liftAndCatchIO $ encryptIO sessionKey (BSL.toStrict . encode $ session)
    let cookie = def { setCookieName = sessionCookieName
                     , setCookieValue = encrypted
                     , setCookiePath = Just "/"
                     }
    setCookie cookie

vaultKey :: Vault.Key Session
vaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE vaultKey #-}

sessionKey :: CS.Key
sessionKey = unsafePerformIO $ CS.getKeyEnv "SESSION_KEY"
{-# NOINLINE sessionKey #-}

sessionMiddleware :: Middleware
sessionMiddleware app request =
    case findSession of
      Just session -> app $ request { vault = Vault.insert vaultKey session (vault request) }
      Nothing -> app request
  where
    findSession :: Maybe Session
    findSession = do
        cookieHeader <- lookup hCookie $ requestHeaders request
        sessionCookie <- lookup sessionCookieName $ parseCookies cookieHeader
        session <- decrypt sessionKey sessionCookie
        decodeStrict session

getSession :: S.ActionM (Maybe Session)
getSession = do
    request <- S.request
    return $ Vault.lookup vaultKey (vault request)

deleteSession :: S.ActionM ()
deleteSession = deleteCookie sessionCookieName
