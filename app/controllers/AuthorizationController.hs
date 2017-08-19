{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module AuthorizationController
    ( app
    ) where

import           Data.Aeson.Types (FromJSON)
import           Data.ByteString (ByteString)
import           Data.Function ((&))
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Text.Encoding as E
import           Database.PostgreSQL.Simple (Connection)
import           GHC.Generics (Generic)
import           Network.OAuth.OAuth2 (ExchangeToken(..), OAuth2(..), OAuth2Result, appendQueryParams, authGetJSON, authorizationUrl, fetchAccessToken, idToken, idtoken, parseOAuth2Error, accessToken)
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

googleKey :: HasGoogleApiKeys a => a -> OAuth2
googleKey context = OAuth2
    { oauthClientId = E.decodeUtf8 $ getGoogleClientId context
    , oauthClientSecret = E.decodeUtf8 $ getGoogleClientSecret context
    , oauthCallback = Just $ getGoogleRedirectUri context
    , oauthOAuthorizeEndpoint = getGoogleOAuthUri context
    , oauthAccessTokenEndpoint = getGoogleAccessTokenUri context
    }

getGoogleLoginUrl :: HasGoogleApiKeys a => a -> ByteString
getGoogleLoginUrl context =
    googleKey context
        & authorizationUrl
        & appendQueryParams [("scope", "email profile")]
        & serializeURIRef'

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
        AuthViews.login $ getGoogleLoginUrl appContext

    S.get "/logout" $ do
        _ <- deleteSession
        S.redirect "/login"

    S.get "/oauth/google" $ do
        code <- S.param "code"
        googleInfoResult <- S.liftAndCatchIO $ getGoogleInfo appContext code
        case googleInfoResult of
            Right googleInfo -> do
                user <- S.liftAndCatchIO $ getOrCreateUser conn googleInfo
                _ <- setSession . Session $ User._userId user
                S.redirect "/"
            Left errors -> do
                logError $ show errors
                S.redirect "/"

getGoogleInfo :: AppContext -> Text -> IO (OAuth2Result Errors GoogleInfo)
getGoogleInfo appContext code = do
    oAuth2Result <- fetchAccessToken (getHttpManager appContext) (googleKey appContext) (ExchangeToken code)

    case (oAuth2Result, idToken <$> oAuth2Result) of
        (Right token, Right (Just jwt)) ->
            getGoogleTokenInfoUri appContext
                & appendQueryParams [("id_token", E.encodeUtf8 $ idtoken jwt)]
                & authGetJSON (getHttpManager appContext) (accessToken token)
        (Left errors, _) ->
            return $ Left errors
        _ ->
            return . Left $ parseOAuth2Error "InvalidRequest"

getOrCreateUser :: Connection -> GoogleInfo -> IO User
getOrCreateUser conn googleInfo = do
    foundUser <- OAuthLogin.findUser conn "google" $ sub googleInfo
    case foundUser of
      Just user ->
          return user
      Nothing ->
          createUser conn googleInfo

createUser :: Connection -> GoogleInfo -> IO User
createUser conn googleInfo = do
    user <- User.create conn (name googleInfo) $ email googleInfo
    _ <- OAuthLogin.create conn (User._userId user) "google" $ sub googleInfo
    return user
