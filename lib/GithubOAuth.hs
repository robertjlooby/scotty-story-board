{-# LANGUAGE OverloadedStrings #-}

module GithubOAuth where

import           Control.Lens ((^.))
import           Data.Aeson.Types (FromJSON(..), (.:), withObject)
import           Data.ByteString (ByteString)
import           Data.Function ((&))
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Text.Encoding as E
import           Database.PostgreSQL.Simple (Connection)
import           Network.OAuth.OAuth2 (ExchangeToken(..), OAuth2(..), OAuth2Result, appendQueryParams, authGetJSON, authorizationUrl, fetchAccessToken, accessToken)
import           Network.OAuth.OAuth2.TokenRequest (Errors)
import           URI.ByteString (serializeURIRef')

import           AppContext (AppContext, HasGithubApiKeys(..), getHttpManager)
import qualified OAuthLogin
import           OpaleyeUtils (runFindQuery)
import qualified User
import           User (User, userId)

data GithubInfo = GithubInfo
    { id_ :: Int
    , email :: Maybe T.Text
    , name :: T.Text
    } deriving (Eq, Show)

instance FromJSON GithubInfo where
    parseJSON = withObject "GithubInfo" $ \v -> GithubInfo
        <$> v .: "id"
        <*> v .: "email"
        <*> v .: "name"

githubKey :: HasGithubApiKeys a => a -> OAuth2
githubKey context = OAuth2
    { oauthClientId = E.decodeUtf8 $ getGithubClientId context
    , oauthClientSecret = E.decodeUtf8 $ getGithubClientSecret context
    , oauthCallback = Just $ getGithubRedirectUri context
    , oauthOAuthorizeEndpoint = getGithubOAuthUri context
    , oauthAccessTokenEndpoint = getGithubAccessTokenUri context
    }

getGithubLoginUrl :: HasGithubApiKeys a => a -> ByteString
getGithubLoginUrl context =
    githubKey context
        & authorizationUrl
        & appendQueryParams [("scope", "user")]
        & serializeURIRef'


getGithubInfo :: AppContext -> Text -> IO (OAuth2Result Errors GithubInfo)
getGithubInfo appContext code = do
    oAuth2Result <- fetchAccessToken (getHttpManager appContext) (githubKey appContext) (ExchangeToken code)

    case oAuth2Result of
        Right token -> do
            putStrLn "aaaaaaaaaaaaa"
            print token
            putStrLn "bbbbbbbbbbbbb"
            getGithubUsersUri appContext
                & authGetJSON (getHttpManager appContext) (accessToken token)
        Left errors ->
            return $ Left errors

getOrCreateGithubUser :: Connection -> GithubInfo -> IO User
getOrCreateGithubUser conn githubInfo = do
    foundUser <- runFindQuery conn $ OAuthLogin.findUserQuery "github" $ T.pack $ show $ id_ githubInfo

    case foundUser of
      Just user ->
          return user
      Nothing ->
          createGithubUser conn githubInfo

createGithubUser :: Connection -> GithubInfo -> IO User
createGithubUser conn githubInfo = do
    user <- User.create conn (name githubInfo) $ fromMaybe "" $ email githubInfo
    _ <- OAuthLogin.create conn (user^.userId) "github" $ T.pack $ show $ id_ githubInfo
    return user