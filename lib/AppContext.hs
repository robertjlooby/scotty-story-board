module AppContext
    ( AppContext(..)
    , getContext
    ) where

import           Configuration.Dotenv (loadFile, onMissingFile)
import qualified Data.ByteString.Char8 as BS
import           Data.Monoid ((<>))
import           System.Environment (getEnv)

data AppContext = AppContext
    { environment :: String
    , port :: Int
    , googleClientId :: BS.ByteString
    , googleClientSecret :: BS.ByteString
    } deriving (Eq, Show)

getContext :: String -> IO AppContext
getContext env = do
    let filename = "config/" <> env <> ".env"
    onMissingFile
        (loadFile False filename)
        (putStrLn $ "Could not find file: " <> filename)

    AppContext
        <$> getEnv "APP_ENV"
        <*> (read <$> getEnv "PORT")
        <*> (BS.pack <$> getEnv "GOOGLE_CLIENT_ID")
        <*> (BS.pack <$> getEnv "GOOGLE_CLIENT_SECRET")
