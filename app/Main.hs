{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson (decodeStrict)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import           Data.Vault.Lazy (Key, newKey, insert)
import qualified Data.Vault.Lazy as V
import           Database.PostgreSQL.Simple (connectPostgreSQL)
import           Network.HTTP.Conduit (newManager, tlsManagerSettings)
import           Network.HTTP.Types.Header (RequestHeaders, hCookie)
import           Network.Wai (Middleware, requestHeaders, vault)
import           Network.Wai.Middleware.ForceSSL (forceSSL)
import           Network.Wai.Middleware.MethodOverridePost (methodOverridePost)
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Network.Wai.Middleware.Static (addBase, staticPolicy)
import           System.Environment (getEnv)
import qualified Web.ClientSession as CS
import           Web.Cookie (parseCookies)
import qualified Web.Scotty as S

import qualified App
import           AppContext (getContext, environment, key, port)
import qualified Auth
import qualified Index
import           Session (Session)

sslMiddleware :: String -> S.ScottyM ()
sslMiddleware "production" = S.middleware forceSSL
sslMiddleware _ = return ()

main :: IO ()
main = do
    appContext <- getContext
    conn <- BS8.pack <$> getEnv "DATABASE_URL" >>= connectPostgreSQL
    mgr <- newManager tlsManagerSettings
    vaultKey <- newKey
    S.scotty (port appContext) $ do
        S.middleware methodOverridePost
        S.middleware logStdout
        sslMiddleware (environment appContext)
        S.middleware $ staticPolicy (addBase "app/static")
        S.middleware (sessionMiddleware (key appContext) vaultKey)
        S.get "/" $ do
            r <- S.request
            S.liftAndCatchIO $ print $ V.lookup vaultKey (vault r)
            S.html "hello"
        --Index.app
        Auth.app conn mgr appContext
        App.app conn

sessionMiddleware :: CS.Key -> Key Session -> Middleware
sessionMiddleware sessionKey vaultKey app req = do
    app req'
  where
    headers = requestHeaders req
    cookieHeaderMaybe = lookup hCookie headers
    maybeCookies = parseCookies <$> cookieHeaderMaybe
    maybeCookie = maybeCookies >>= lookup "session"
    maybeSessionBytes = maybeCookie >>= CS.decrypt sessionKey
    maybeSession = maybeSessionBytes >>= decodeStrict
    withSession (Just session) = req { vault = insert vaultKey session (vault req) }
    withSession _ = req
    req' = withSession maybeSession
