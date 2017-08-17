{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

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
import           Data.Maybe (listToMaybe)
import           Data.Monoid ((<>))
import           Data.Profunctor.Product (p2)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Text (Text)
import           Database.PostgreSQL.Simple (Connection)
import           Opaleye (Column, PGInt4, PGText, Query, Table(Table), (.===), (.==), optional, pgInt4, pgStrictText, queryTable, required, restrict, runDelete, runInsertMany, runInsertManyReturning, runQuery, runUpdate)
import           Text.Blaze (ToValue, toValue)

import           OpaleyeUtils (withId)
import           User (UserId, UserIdColumn, userIdColumn, userQuery)
import qualified User

newtype ProjectId' a = ProjectId a deriving (Eq, Ord, Show)
type ProjectId = ProjectId' Int
type ProjectIdColumn = ProjectId' (Column PGInt4)
type ProjectIdColumnMaybe = ProjectId' (Maybe (Column PGInt4))
$(makeAdaptorAndInstance "pProjectId" ''ProjectId')

instance Functor ProjectId' where
    fmap f (ProjectId a) = ProjectId (f a)

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

instance ToValue a => ToValue (Project' a b c) where
    toValue = toValue . id_

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

projectsUsersTable :: Table (ProjectIdColumn, UserIdColumn) (ProjectIdColumn, UserIdColumn)
projectsUsersTable = Table "projects_users"
                           (p2 ( pProjectId (ProjectId (required "project_id"))
                               , userIdColumn (required "user_id")))

projectsUsersQuery :: Query (ProjectIdColumn, UserIdColumn)
projectsUsersQuery = queryTable projectsUsersTable

create :: Connection -> Text -> Text -> IO Project
create conn name' description' = do
    [project] <- runInsertManyReturning conn projectsTable [Project (ProjectId Nothing) (pgStrictText name') (pgStrictText description')] id
    return project

addUser :: Connection -> ProjectId -> UserId -> IO ()
addUser conn projectId userId = do
    _ <- runInsertMany conn projectsUsersTable [(pgInt4 <$> projectId, pgInt4 <$> userId)]
    return ()

find :: Connection -> ProjectId -> IO (Maybe Project)
find conn projectId = do
    listToMaybe <$> runProjectQuery conn (findQuery projectId)

findQuery :: ProjectId -> Query ProjectColumnRead
findQuery projectId = proc () -> do
    row <- projectQuery -< ()
    withId projectId -< id_ row
    returnA -< row

findByName :: Connection -> Text -> IO (Maybe Project)
findByName conn projectName = do
    listToMaybe <$> runProjectQuery conn (findByNameQuery projectName)

findByNameQuery :: Text -> Query ProjectColumnRead
findByNameQuery projectName = proc () -> do
    row <- projectQuery -< ()
    restrict -< name row .== pgStrictText projectName
    returnA -< row

findByUserId :: Connection -> UserId -> ProjectId -> IO (Maybe Project)
findByUserId conn userId projectId = do
    listToMaybe <$> runProjectQuery conn (findByUserIdQuery userId projectId)

findByUserIdQuery :: UserId -> ProjectId -> Query ProjectColumnRead
findByUserIdQuery userId projectId = proc () -> do
    user <- userQuery -< ()
    project <- projectQuery -< ()
    (puProjectId, puUserId) <- projectsUsersQuery -< ()

    withId userId -< User.id_ user
    withId userId -< puUserId
    withId projectId -< id_ project
    restrict -< id_ project .=== puProjectId

    returnA -< project

allByUserId :: Connection -> UserId -> IO [Project]
allByUserId conn userId =
    runProjectQuery conn (allByUserIdQuery userId)

allByUserIdQuery :: UserId -> Query ProjectColumnRead
allByUserIdQuery userId = proc () -> do
    user <- userQuery -< ()
    project <- projectQuery -< ()
    (puProjectId, puUserId) <- projectsUsersQuery -< ()

    withId userId -< User.id_ user
    withId userId -< puUserId
    restrict -< id_ project .=== puProjectId

    returnA -< project

update :: Connection -> Project -> IO ()
update conn project = do
    _ <- runUpdate
             conn
             projectsTable
             (\p -> p {id_ = Just <$> id_ p, name = pgStrictText (name project), description = pgStrictText (description project)})
             (\p -> id_ p .=== (pgInt4 <$> id_ project))
    return ()

delete :: Connection -> ProjectId -> IO ()
delete conn projectId = do
    _ <- runDelete conn projectsTable (\p -> id_ p .=== (pgInt4 <$> projectId))
    return ()
