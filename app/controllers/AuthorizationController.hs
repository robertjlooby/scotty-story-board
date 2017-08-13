{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module AuthorizationController
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
import           Network.HTTP.Simple (getResponseBody, httpJSON, setRequestQueryString)
import           Network.HTTP.Types (renderSimpleQuery)
import           Network.OAuth.OAuth2 (ExchangeToken(..), OAuth2(..), OAuth2Error, fetchAccessToken, idToken, idtoken, uriToRequest)
import           Network.OAuth.OAuth2.TokenRequest (Errors)
import           URI.ByteString (serializeURIRef')
import qualified Web.Scotty as S

import           AppContext (AppContext, HasGoogleApiKeys(..), getDbConn, getHttpManager)
import qualified AuthViews
import qualified OAuthLogin
import           Session (Session(Session), deleteSession, setSession)
import qualified User
import           User (User)
import           Util (logError)

googleKey :: AppContext -> OAuth2
googleKey appContext = OAuth2
    { oauthClientId = E.decodeUtf8 $ getGoogleClientId appContext
    , oauthClientSecret = E.decodeUtf8 $ getGoogleClientSecret appContext
    , oauthCallback = Just $ getGoogleRedirectUri appContext
    , oauthOAuthorizeEndpoint = getGoogleOAuthUri appContext
    , oauthAccessTokenEndpoint = getGoogleAccessTokenUri appContext
    }

getGoogleLoginUrl :: AppContext -> ByteString
getGoogleLoginUrl appContext =
    serializeURIRef' (getGoogleOAuthUri appContext)
        <> renderSimpleQuery
            True
            [ ("client_id", getGoogleClientId appContext)
            , ("response_type", "code")
            , ("redirect_uri", serializeURIRef' (getGoogleRedirectUri appContext))
            , ("scope", "email profile")
            ]

data GoogleInfo = GoogleInfo
    { sub :: T.Text
    , email :: T.Text
    , name :: T.Text
    } deriving (Eq, Generic, Show)

instance FromJSON GoogleInfo

app :: AppContext -> S.ScottyM ()
app appContext = do
    let conn = getDbConn appContext
    S.get "/login" $ do
        AuthViews.login (getGoogleLoginUrl appContext)

    S.get "/logout" $ do
        _ <- deleteSession
        S.redirect "/login"

    S.get "/oauth/google" $ do
        code <- S.param "code"
        googleInfoResult <- S.liftAndCatchIO $ getGoogleInfo appContext code
        case googleInfoResult of
            Right googleInfo -> do
                user <- S.liftAndCatchIO $ getOrCreateUser conn googleInfo
                _ <- setSession . Session . User.id_ $ user
                S.redirect "/"
            Left errors -> do
                logError $ show errors
                S.redirect "/"

getGoogleInfo :: AppContext -> Text -> IO (Either (OAuth2Error Errors) GoogleInfo)
getGoogleInfo appContext code = do
    oAuth2Result <- fetchAccessToken (getHttpManager appContext) (googleKey appContext) (ExchangeToken code)
    case oAuth2Result of
        Right token -> do
            (Just jwt) <- return . idToken $ token
            req <- uriToRequest . getGoogleTokenInfoUri $ appContext
            resp <- httpJSON (setRequestQueryString [("id_token", Just (E.encodeUtf8 $ idtoken jwt))] req)
            return . Right $ getResponseBody resp
        Left errors -> return . Left $ errors

getOrCreateUser :: Connection -> GoogleInfo -> IO User
getOrCreateUser conn googleInfo = do
    foundUser <- OAuthLogin.findUser conn "google" (sub googleInfo)
    case foundUser of
      Just user -> return user
      Nothing -> createUser conn googleInfo

createUser :: Connection -> GoogleInfo -> IO User
createUser conn googleInfo = do
    user <- User.create conn (name googleInfo) (email googleInfo)
    _ <- OAuthLogin.create conn (User.id_ user) "google" (sub googleInfo)
    return user
