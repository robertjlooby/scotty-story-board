{-# LANGUAGE OverloadedStrings #-}

module Project where

import Control.Exception (throwIO)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection, FromRow, Only(..), query)
import Database.PostgreSQL.Simple.FromRow (field, fromRow)
import SQLExceptions (DuplicateData(..))

newtype ProjectId = ProjectId Int deriving (Eq, Ord, Show)

instance FromRow ProjectId where
  fromRow = ProjectId <$> field

data Project = Project
    { name :: Text
    , description :: Text
    } deriving (Eq, Show)

create :: Connection -> Text -> Text -> IO ProjectId
create conn name' description' = do
  ids <- query conn "INSERT INTO projects (name, description) VALUES (?, ?) RETURNING id" (name', description')
  case ids of
    [(Only projectId)] -> return $ ProjectId projectId
    _ -> throwIO DuplicateData

find :: Connection -> ProjectId -> IO (Maybe Project)
find conn (ProjectId projectId) = do
  project <- query conn "SELECT name, description FROM projects WHERE id = ?" $ Only projectId
  case project of
    [(name', description')] -> return $ Just $ Project name' description'
    _ -> return Nothing

findByName :: Connection -> Text -> IO (Maybe Project)
findByName conn projectName = do
  project <- query conn "SELECT name, description FROM projects WHERE name = ?" $ Only projectName
  case project of
    [(name', description')] -> return $ Just $ Project name' description'
    _ -> return Nothing
