module AppContext
    ( AppContext
    , HasDbConn(..)
    , HasEnvironment(..)
    , HasGoogleApiKeys(..)
    , HasHttpManager(..)
    , HasPort(..)
    , getContext
    ) where

import           Configuration.Dotenv (loadFile, onMissingFile)
import qualified Data.ByteString.Char8 as BS8
import           Data.Monoid ((<>))
import           Database.PostgreSQL.Simple (Connection, connectPostgreSQL)
import           Network.HTTP.Conduit (newManager, tlsManagerSettings)
import           Network.HTTP.Client (HasHttpManager(..), Manager)
import           System.Environment (getEnv)

data AppContext = AppContext
    { environment :: String
    , port :: Int
    , dbConn :: Connection
    , googleClientId :: BS8.ByteString
    , googleClientSecret :: BS8.ByteString
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

class HasGoogleApiKeys a where
    getGoogleClientId :: a -> BS8.ByteString
    getGoogleClientSecret :: a -> BS8.ByteString
instance HasGoogleApiKeys AppContext where
    getGoogleClientId = googleClientId
    getGoogleClientSecret = googleClientSecret

instance HasHttpManager AppContext where
    getHttpManager = httpManager

class HasPort a where
    getPort :: a -> Int
instance HasPort Int where
    getPort = id
instance HasPort AppContext where
    getPort = port

getContext :: String -> IO AppContext
getContext env = do
    let filename = "config/" <> env <> ".env"
    onMissingFile
        (loadFile False filename)
        (putStrLn $ "Could not find file: " <> filename)

    AppContext
        <$> getEnv "APP_ENV"
        <*> (read <$> getEnv "PORT")
        <*> (BS8.pack <$> getEnv "DATABASE_URL" >>= connectPostgreSQL)
        <*> (BS8.pack <$> getEnv "GOOGLE_CLIENT_ID")
        <*> (BS8.pack <$> getEnv "GOOGLE_CLIENT_SECRET")
        <*> newManager tlsManagerSettings
