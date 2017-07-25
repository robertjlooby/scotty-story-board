module Main where

import qualified Data.ByteString.Char8 as BS8
import           Database.PostgreSQL.Simple ( connectPostgreSQL, withTransaction)
import           Database.PostgreSQL.Simple.Migration (MigrationCommand(..), MigrationContext(..), MigrationResult, runMigration)
import           System.Environment (getEnv)

main :: IO (MigrationResult String)
main = do
    dbUrl <- BS8.pack <$> getEnv "DATABASE_URL"
    conn <- connectPostgreSQL dbUrl
    withTransaction conn $ do
        _ <- runMigration $ MigrationContext MigrationInitialization True conn
        runMigration $ MigrationContext (MigrationDirectory "db/migrations") True conn
