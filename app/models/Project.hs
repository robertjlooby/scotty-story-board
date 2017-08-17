{-# LANGUAGE Arrows                     #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Project
    (
    -- * Types
      Project
    , ProjectId'(..)
    , ProjectId
    -- * Accessors
    , id_
    , name
    , description
    -- * Queries
    , create
    , find
    , findByName
    , findByUserId
    , allByUserId
    , addUser
    , update
    , delete
    ) where

import           Control.Arrow (returnA)
import           Data.Monoid ((<>))
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Text (Text)
import           Database.PostgreSQL.Simple (Connection, FromRow, Only(..), execute, query)
import           Database.PostgreSQL.Simple.FromField (FromField)
import           Database.PostgreSQL.Simple.FromRow (field, fromRow)
import           Database.PostgreSQL.Simple.ToField (ToField)
import           Opaleye (Column, PGInt4, PGText, Query, Table(Table), (.===), (.==), optional, pgInt4, pgStrictText, queryTable, required, restrict, runDelete, runInsertManyReturning, runQuery, runUpdate)
import           Text.Blaze (ToValue, toValue)

import           User (UserId)

newtype ProjectId' a = ProjectId a deriving (Eq, FromField, Ord, Show, ToField)
type ProjectId = ProjectId' Int
type ProjectIdColumn = ProjectId' (Column PGInt4)
type ProjectIdColumnMaybe = ProjectId' (Maybe (Column PGInt4))
$(makeAdaptorAndInstance "pProjectId" ''ProjectId')

instance Functor ProjectId' where
    fmap f (ProjectId a) = ProjectId (f a)

instance FromField a => FromRow (ProjectId' a) where
    fromRow = ProjectId <$> field

instance ToValue a => ToValue (ProjectId' a) where
    toValue (ProjectId projectId) = "/projects/" <> toValue projectId

data Project' a b c = Project
    { id_ :: a
    , name :: b
    , description :: c
    } deriving (Eq, Show)
type Project = Project' ProjectId Text Text
type ProjectColumnWrite = Project' ProjectIdColumnMaybe (Column PGText) (Column PGText)
type ProjectColumnRead = Project' ProjectIdColumn (Column PGText) (Column PGText)
$(makeAdaptorAndInstance "pProject" ''Project')

projectsTable :: Table ProjectColumnWrite ProjectColumnRead
projectsTable = Table "projects"
                      (pProject Project { id_ = pProjectId (ProjectId (optional "id"))
                                        , name = required "name"
                                        , description = required "description"
                                        })

projectQuery :: Query ProjectColumnRead
projectQuery = queryTable projectsTable

runProjectQuery :: Connection -> Query ProjectColumnRead -> IO [Project]
runProjectQuery = runQuery

instance (FromField a, FromField b, FromField c) => FromRow (Project' a b c) where
    fromRow = Project <$> field <*> field <*> field

instance ToValue a => ToValue (Project' a b c) where
    toValue = toValue . id_

create :: Connection -> Text -> Text -> IO Project
create conn name' description' = do
    [project] <- runInsertManyReturning conn projectsTable [Project (ProjectId Nothing) (pgStrictText name') (pgStrictText description')] id
    return project

addUser :: Connection -> ProjectId -> UserId -> IO ()
addUser conn projectId userId = do
    _ <- execute conn "INSERT INTO projects_users (project_id, user_id) VALUES (?, ?)" (projectId, userId)
    return ()

find :: Connection -> ProjectId -> IO (Maybe Project)
find conn projectId = do
    projects <- runProjectQuery conn (findQuery projectId)
    case projects of
        [project] -> return $ Just project
        _         -> return Nothing

findQuery :: ProjectId -> Query ProjectColumnRead
findQuery projectId = proc () -> do
    row <- projectQuery -< ()
    restrict -< id_ row .=== (pgInt4 <$> projectId)
    returnA -< row

findByName :: Connection -> Text -> IO (Maybe Project)
findByName conn projectName = do
    projects <- runProjectQuery conn (findByNameQuery projectName)
    case projects of
        [project] -> return $ Just project
        _         -> return Nothing

findByNameQuery :: Text -> Query ProjectColumnRead
findByNameQuery projectName = proc () -> do
    row <- projectQuery -< ()
    restrict -< name row .== pgStrictText projectName
    returnA -< row

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
    _ <- runUpdate conn projectsTable (\p -> p {id_ = Just <$> id_ p, name = pgStrictText (name project), description = pgStrictText (description project)}) (\p -> id_ p .=== (pgInt4 <$> id_ project))
    return ()

delete :: Connection -> ProjectId -> IO ()
delete conn projectId = do
    _ <- runDelete conn projectsTable (\p -> id_ p .=== (pgInt4 <$> projectId))
    return ()
