{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Project where

import           Data.Text (Text)
import           Database.PostgreSQL.Simple (Connection, FromRow, Only(..), execute, query, query_)
import           Database.PostgreSQL.Simple.FromField (FromField)
import           Database.PostgreSQL.Simple.FromRow (field, fromRow)
import           Database.PostgreSQL.Simple.ToField (ToField)

newtype ProjectId = ProjectId Int deriving (Eq, FromField, Ord, Show, ToField)

instance FromRow ProjectId where
    fromRow = ProjectId <$> field

data Project = Project
    { id_ :: ProjectId
    , name :: Text
    , description :: Text
    } deriving (Eq, Show)

instance FromRow Project where
    fromRow = Project <$> field <*> field <*> field

create :: Connection -> Text -> Text -> IO ProjectId
create conn name' description' = do
    [projectId] <- query conn "INSERT INTO projects (name, description) VALUES (?, ?) RETURNING id" (name', description')
    return projectId

find :: Connection -> ProjectId -> IO (Maybe Project)
find conn (ProjectId projectId) = do
    project <- query conn "SELECT id, name, description FROM projects WHERE id = ?" $ Only projectId
    case project of
        [proj] -> return $ Just proj
        _      -> return Nothing

findByName :: Connection -> Text -> IO (Maybe Project)
findByName conn projectName = do
    project <- query conn "SELECT id, name, description FROM projects WHERE name = ?" $ Only projectName
    case project of
        [proj] -> return $ Just proj
        _      -> return Nothing

update :: Connection -> Project -> IO ()
update conn project = do
    _ <- execute conn "UPDATE projects SET name = ?, description = ? WHERE id = ?" (name project, description project, id_ project)
    return ()

findAll :: Connection -> IO [Project]
findAll conn =
    query_ conn "SELECT id, name, description FROM projects"

delete :: Connection -> ProjectId -> IO ()
delete conn projectId = do
    _ <- execute conn "DELETE FROM projects WHERE id = ?" (Only projectId)
    return ()
