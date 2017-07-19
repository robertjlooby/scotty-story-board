module AppContext
    ( AppContext(..)
    , getContext
    ) where

import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)

data AppContext = AppContext
    { environment :: String
    , port :: Int
    , googleClientId :: BS.ByteString
    , googleClientSecret :: BS.ByteString
    } deriving (Eq, Show)

getEnvWithDefault :: String -> String -> IO String
getEnvWithDefault envName def = fromMaybe def <$> lookupEnv envName

getContext :: IO AppContext
getContext = do
    AppContext
        <$> getEnvWithDefault "APP_ENV" "development"
        <*> (read <$> getEnvWithDefault "PORT" "3000")
        <*> (BS.pack <$> getEnvWithDefault "GOOGLE_CLIENT_ID" "googleClientId")
        <*> (BS.pack <$> getEnvWithDefault "GOOGLE_CLIENT_SECRET" "googleClientSecret")
