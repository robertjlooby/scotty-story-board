module AppContext
    ( AppContext
    , HasDbConn(..)
    , HasEnvironment(..)
    , HasGithubApiKeys(..)
    , HasGoogleApiKeys(..)
    , HasHttpManager(..)
    , HasPort(..)
    , getContext
    ) where

import           Configuration.Dotenv (loadFile, onMissingFile)
import qualified Data.ByteString.Char8 as BS8
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Database.PostgreSQL.Simple (Connection, connectPostgreSQL)
import           Network.HTTP.Conduit (newManager, tlsManagerSettings)
import           Network.HTTP.Client (HasHttpManager(..), Manager)
import           System.Environment (getEnv, lookupEnv)
import           URI.ByteString (URI, parseURI, strictURIParserOptions)

data AppContext = AppContext
    { environment :: String
    , port :: Int
    , dbConn :: Connection
    , githubClientId :: BS8.ByteString
    , githubClientSecret :: BS8.ByteString
    , githubAccessTokenUri :: URI
    , githubOAuthUri :: URI
    , githubRedirectUri :: URI
    , githubUsersUri :: URI
    , googleClientId :: BS8.ByteString
    , googleClientSecret :: BS8.ByteString
    , googleAccessTokenUri :: URI
    , googleOAuthUri :: URI
    , googleRedirectUri :: URI
    , googleTokenInfoUri :: URI
    , httpManager :: Manager
    }

class HasDbConn a where
    getDbConn :: a -> Connection
instance HasDbConn Connection where
    getDbConn = id
instance HasDbConn AppContext where
    getDbConn = dbConn

class HasEnvironment a where
    getEnvironment :: a -> String
instance HasEnvironment AppContext where
    getEnvironment = environment

class HasGithubApiKeys a where
    getGithubClientId :: a -> BS8.ByteString
    getGithubClientSecret :: a -> BS8.ByteString
    getGithubAccessTokenUri :: a -> URI
    getGithubOAuthUri :: a -> URI
    getGithubRedirectUri :: a -> URI
    getGithubUsersUri :: a -> URI
instance HasGithubApiKeys AppContext where
    getGithubClientId = githubClientId
    getGithubClientSecret = githubClientSecret
    getGithubAccessTokenUri = githubAccessTokenUri
    getGithubOAuthUri = githubOAuthUri
    getGithubRedirectUri = githubRedirectUri
    getGithubUsersUri = githubUsersUri

class HasGoogleApiKeys a where
    getGoogleClientId :: a -> BS8.ByteString
    getGoogleClientSecret :: a -> BS8.ByteString
    getGoogleAccessTokenUri :: a -> URI
    getGoogleOAuthUri :: a -> URI
    getGoogleRedirectUri :: a -> URI
    getGoogleTokenInfoUri :: a -> URI
instance HasGoogleApiKeys AppContext where
    getGoogleClientId = googleClientId
    getGoogleClientSecret = googleClientSecret
    getGoogleAccessTokenUri = googleAccessTokenUri
    getGoogleOAuthUri = googleOAuthUri
    getGoogleRedirectUri = googleRedirectUri
    getGoogleTokenInfoUri = googleTokenInfoUri

instance HasHttpManager AppContext where
    getHttpManager = httpManager

class HasPort a where
    getPort :: a -> Int
instance HasPort Int where
    getPort = id
instance HasPort AppContext where
    getPort = port

getContext :: String -> IO AppContext
getContext defaultEnv = do
    currentEnv <- fromMaybe defaultEnv <$> lookupEnv "APP_ENV"
    let filename = "config/" <> currentEnv <> ".env"
    onMissingFile
        (loadFile False filename)
        (putStrLn $ "Could not find file: " <> filename)

    AppContext
        <$> getEnv "APP_ENV"
        <*> (read <$> getEnv "PORT")
        <*> (BS8.pack <$> getEnv "DATABASE_URL" >>= connectPostgreSQL)
        <*> (BS8.pack <$> getEnv "GITHUB_CLIENT_ID")
        <*> (BS8.pack <$> getEnv "GITHUB_CLIENT_SECRET")
        <*> getUri "GITHUB_ACCESS_TOKEN_URI"
        <*> getUri "GITHUB_OAUTH_URI"
        <*> getUri "GITHUB_REDIRECT_URI"
        <*> getUri "GITHUB_USERS_URI"
        <*> (BS8.pack <$> getEnv "GOOGLE_CLIENT_ID")
        <*> (BS8.pack <$> getEnv "GOOGLE_CLIENT_SECRET")
        <*> getUri "GOOGLE_ACCESS_TOKEN_URI"
        <*> getUri "GOOGLE_OAUTH_URI"
        <*> getUri "GOOGLE_REDIRECT_URI"
        <*> getUri "GOOGLE_TOKEN_INFO_URI"
        <*> newManager tlsManagerSettings

getUri :: String -> IO URI
getUri varName = do
    eitherUri <- parseURI strictURIParserOptions <$> BS8.pack <$> getEnv varName

    case eitherUri of
      Left err -> fail $ "Failed to parse " ++ varName ++ ": " ++ show err
      Right uri -> return uri
