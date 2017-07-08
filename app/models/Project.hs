{-# LANGUAGE OverloadedStrings #-}

module Models.Project where

import Control.Exception (throwIO)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection, FromRow, Only(..), query, query_)
import Database.PostgreSQL.Simple.FromRow (field, fromRow)
import SQLExceptions (DuplicateData(..))

newtype ProjectId = ProjectId Int deriving (Eq, Ord, Show)

instance FromRow ProjectId where
  fromRow = ProjectId <$> field

data Project = Project
    { name :: Text
    , description :: Text
    } deriving (Eq, Show)

instance FromRow Project where
  fromRow = Project <$> field <*> field

create :: Connection -> Text -> Text -> IO ProjectId
create conn name' description' = do
  ids <- query conn "INSERT INTO projects (name, description) VALUES (?, ?) RETURNING id" (name', description')
  case ids of
    [projectId] -> return projectId
    _           -> throwIO DuplicateData

find :: Connection -> ProjectId -> IO (Maybe Project)
find conn (ProjectId projectId) = do
  project <- query conn "SELECT name, description FROM projects WHERE id = ?" $ Only projectId
  case project of
    [proj] -> return $ Just proj
    _      -> return Nothing

findByName :: Connection -> Text -> IO (Maybe Project)
findByName conn projectName = do
  project <- query conn "SELECT name, description FROM projects WHERE name = ?" $ Only projectName
  case project of
    [proj] -> return $ Just proj
    _      -> return Nothing

findAll :: Connection -> IO [Project]
findAll conn =
  query_ conn "SELECT name, description FROM projects"
