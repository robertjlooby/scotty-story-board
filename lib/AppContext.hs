module AppContext
    ( AppContext(..)
    , HasDbConn(..)
    , getContext
    ) where

import           Configuration.Dotenv (loadFile, onMissingFile)
import qualified Data.ByteString.Char8 as BS8
import           Data.Monoid ((<>))
import           Database.PostgreSQL.Simple (Connection, connectPostgreSQL)
import           System.Environment (getEnv)

data AppContext = AppContext
    { environment :: String
    , port :: Int
    , dbConn :: Connection
    , googleClientId :: BS8.ByteString
    , googleClientSecret :: BS8.ByteString
    }

class HasDbConn a where
    getDbConn :: a -> Connection
instance HasDbConn Connection where
    getDbConn = id
instance HasDbConn AppContext where
    getDbConn = dbConn

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
