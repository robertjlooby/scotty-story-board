{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Project where

import           Data.Monoid ((<>))
import           Data.Text (Text)
import           Database.PostgreSQL.Simple (Connection, FromRow, Only(..), execute, query, query_)
import           Database.PostgreSQL.Simple.FromField (FromField)
import           Database.PostgreSQL.Simple.FromRow (field, fromRow)
import           Database.PostgreSQL.Simple.ToField (ToField)
import           Text.Blaze (ToValue, toValue)

import           User (UserId)

newtype ProjectId = ProjectId Int deriving (Eq, FromField, Ord, Show, ToField)

instance FromRow ProjectId where
    fromRow = ProjectId <$> field

instance ToValue ProjectId where
    toValue (ProjectId projectId) = "/projects/" <> toValue projectId

data Project = Project
    { id_ :: ProjectId
    , name :: Text
    , description :: Text
    } deriving (Eq, Show)

instance FromRow Project where
    fromRow = Project <$> field <*> field <*> field

instance ToValue Project where
    toValue = toValue . id_

create :: Connection -> Text -> Text -> IO Project
create conn name' description' = do
    [project] <- query conn "INSERT INTO projects (name, description) VALUES (?, ?) RETURNING *" (name', description')
    return project

addUser :: Connection -> ProjectId -> UserId -> IO ()
addUser conn projectId userId = do
    _ <- execute conn "INSERT INTO projects_users (project_id, user_id) VALUES (?, ?)" (projectId, userId)
    return ()

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

findByUserId :: Connection -> UserId -> ProjectId -> IO (Maybe Project)
findByUserId conn userId projectId = do
    project <- query conn "SELECT id, name, description FROM projects INNER JOIN projects_users ON projects_users.project_id = projects.id WHERE projects_users.user_id = ? AND projects.id = ?" (userId, projectId)
    case project of
        [proj] -> return $ Just proj
        _      -> return Nothing

allByUserId :: Connection -> UserId -> IO [Project]
allByUserId conn userId =
    query conn "SELECT id, name, description FROM projects INNER JOIN projects_users ON projects_users.project_id = projects.id WHERE projects_users.user_id = ?" (Only userId)

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
