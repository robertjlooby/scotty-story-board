module Main where

import qualified Data.ByteString.Char8 as BS8
import           Database.PostgreSQL.Simple ( connectPostgreSQL, withTransaction)
import           Database.PostgreSQL.Simple.Migration (MigrationCommand(..), MigrationContext(..), MigrationResult, runMigration)
import           System.Environment (getArgs, getEnv)
import           System.Exit (exitFailure)

import           AppContext (getContext)

main :: IO (MigrationResult String)
main = do
    args <- getArgs
    case args of
      [env] -> migrateEnv env
      _ -> do
          putStrLn "Usage: stack exec migrate <env>"
          exitFailure

migrateEnv :: String -> IO (MigrationResult String)
migrateEnv env = do
    _ <- getContext env
    dbUrl <- BS8.pack <$> getEnv "DATABASE_URL"
    conn <- connectPostgreSQL dbUrl
    withTransaction conn $ do
        _ <- runMigration $ MigrationContext MigrationInitialization True conn
        runMigration $ MigrationContext (MigrationDirectory "db/migrations") True conn
