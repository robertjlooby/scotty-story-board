{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Auth
    ( app
    ) where

import           Data.Aeson.Types (FromJSON)
import           Data.ByteString (ByteString)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Text.Encoding as E
import           Database.PostgreSQL.Simple (Connection)
import           GHC.Generics (Generic)
import           Network.HTTP.Conduit (Manager)
import           Network.HTTP.Simple (getResponseBody, httpJSON, parseRequest, setRequestQueryString)
import           Network.HTTP.Types (renderSimpleQuery)
import           Network.OAuth.OAuth2 (ExchangeToken(..), OAuth2(..), OAuth2Error, fetchAccessToken, idToken, idtoken)
import           Network.OAuth.OAuth2.TokenRequest (Errors)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           URI.ByteString (Absolute, URIRef, serializeURIRef')
import           URI.ByteString.QQ (uri)
import qualified Web.Scotty as S

import           AppContext (AppContext, environment, googleClientId, googleClientSecret)
import qualified AuthViews
import qualified OAuthLogin
import           Session (Session(Session), getSession, setSession)
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
            , ("scope", "email profile")
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
    , name :: T.Text
    } deriving (Eq, Generic, Show)

instance FromJSON GoogleInfo

app :: Connection -> Manager -> AppContext -> S.ScottyM ()
app conn mgr appContext = do
    S.get "/login" $ do
        session <- getSession
        S.liftAndCatchIO $ print session
        S.html $ renderHtml $ AuthViews.login (getGoogleLoginUrl appContext)

    S.get "/oauth/google" $ do
        code <- S.param "code" :: S.ActionM T.Text
        googleInfoResult <- S.liftAndCatchIO $ getGoogleInfo mgr appContext code
        case googleInfoResult of
            Right googleInfo -> do
                user <- S.liftAndCatchIO $ getOrCreateUser conn googleInfo
                let session = Session (User.id_ user)
                _ <- setSession session
                S.redirect "/"
            Left errors -> do
                S.liftAndCatchIO $ print errors
                S.redirect "/"

getGoogleInfo :: Manager -> AppContext -> Text -> IO (Either (OAuth2Error Errors) GoogleInfo)
getGoogleInfo mgr appContext code = do
    oAuth2Result <- fetchAccessToken mgr (googleKey appContext) (ExchangeToken code)
    case oAuth2Result of
        Right token -> do
            (Just jwt) <- return . idToken $ token
            req <- parseRequest "https://www.googleapis.com/oauth2/v3/tokeninfo"
            resp <- httpJSON (setRequestQueryString [("id_token", Just (E.encodeUtf8 $ idtoken jwt))] req)
            return $ Right $ getResponseBody resp
        Left errors -> return $ Left errors

getOrCreateUser :: Connection -> GoogleInfo -> IO User
getOrCreateUser conn googleInfo = do
    foundUser <- OAuthLogin.findUser conn "google" (sub googleInfo)
    case foundUser of
      Just user -> return user
      Nothing -> createUser conn googleInfo

createUser :: Connection -> GoogleInfo -> IO User
createUser conn googleInfo = do
    userId <- User.create conn (name googleInfo) (email googleInfo)
    (Just user) <- User.find conn userId
    _ <- OAuthLogin.create conn userId "google" (sub googleInfo)
    return user
