{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Auth
    ( app
    ) where

import           Data.Aeson (decode)
import           Data.Aeson.Types (FromJSON)
import           Data.ByteString (ByteString)
import           Data.ByteString.Lazy (fromStrict)
import           Data.ByteString.Base64 (decodeLenient)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Text.Encoding as E
import           Database.PostgreSQL.Simple (Connection)
import           GHC.Generics (Generic)
import           Network.HTTP.Conduit (Manager)
import           Network.HTTP.Types (renderSimpleQuery)
import           Network.OAuth.OAuth2 (ExchangeToken(..), OAuth2(..), fetchAccessToken, idToken, idtoken)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           URI.ByteString (Absolute, URIRef, serializeURIRef')
import           URI.ByteString.QQ (uri)
import qualified Web.Scotty as S

import           AppContext (AppContext, environment, googleClientId, googleClientSecret, key)
import qualified AuthViews
import qualified OAuthLogin
import           Session (getSessionCookie, setSessionCookie)
import qualified User
import           User (User)

googleKey :: AppContext -> OAuth2
googleKey appContext = OAuth2 { oauthClientId = E.decodeUtf8 $ googleClientId appContext
                              , oauthClientSecret = E.decodeUtf8 $ googleClientSecret appContext
                              , oauthCallback = Just (googleCallback (environment appContext))
                              , oauthOAuthorizeEndpoint = [uri|https://accounts.google.com/o/oauth2/auth|]
                              , oauthAccessTokenEndpoint = [uri|https://www.googleapis.com/oauth2/v4/token|]
                              }

getGoogleLoginUrl :: AppContext -> ByteString
getGoogleLoginUrl appContext =
    googleAuthBaseUrl
        <> renderSimpleQuery
            True
            [ ("client_id", googleClientId appContext)
            , ("response_type", "code")
            , ("redirect_uri", googleRedirectUri (environment appContext))
            , ("scope", "openid email")
            ]
  where
    googleAuthBaseUrl = "https://accounts.google.com/o/oauth2/v2/auth"

googleCallback :: String -> URIRef Absolute
googleCallback "production" = [uri|https://scotty-story-board.herokuapp.com/oauth/google|]
googleCallback _            = [uri|http://localhost:3000/oauth/google|]

googleRedirectUri :: String -> ByteString
googleRedirectUri env = serializeURIRef' (googleCallback env)

data GoogleInfo = GoogleInfo
    { sub :: T.Text
    , email :: T.Text
    } deriving (Eq, Generic, Show)

instance FromJSON GoogleInfo

app :: Connection -> Manager -> AppContext -> S.ScottyM ()
app conn mgr appContext = do
    S.get "/login" $ do
        session <- getSessionCookie (key appContext)
        S.liftAndCatchIO $ print session
        S.html $ renderHtml $ AuthViews.login (getGoogleLoginUrl appContext)

    S.get "/oauth/google" $ do
        code <- S.param "code" :: S.ActionM T.Text
        googleInfo <- S.liftAndCatchIO $ getGoogleInfo mgr appContext code
        S.liftAndCatchIO $ putStrLn $ show googleInfo
        user <- S.liftAndCatchIO $ getOrCreateUser conn googleInfo
        S.liftAndCatchIO $ putStrLn $ show user
        _ <- setSessionCookie user (key appContext)
        S.redirect "/"

getGoogleInfo :: Manager -> AppContext -> Text -> IO GoogleInfo
getGoogleInfo mgr appContext code = do
    (Right token) <- fetchAccessToken mgr (googleKey appContext) (ExchangeToken code)
    (Just jwt) <- return . idToken $ token
    let encodedGoogleInfo = T.dropAround ((==) '.') . T.dropAround ((/=) '.') . idtoken $ jwt
        googleInfoJson    = decodeLenient (E.encodeUtf8 encodedGoogleInfo)
        (Just googleInfo) = decode (fromStrict googleInfoJson)
    return googleInfo

getOrCreateUser :: Connection -> GoogleInfo -> IO User
getOrCreateUser conn googleInfo = do
    foundUser <- OAuthLogin.findUser conn "google" (sub googleInfo)
    case foundUser of
      Just user -> return user
      Nothing -> createUser conn googleInfo

createUser :: Connection -> GoogleInfo -> IO User
createUser conn googleInfo = do
    userId <- User.create conn (email googleInfo)
    (Just user) <- User.find conn userId
    _ <- OAuthLogin.create conn userId "google" (sub googleInfo)
    return user
