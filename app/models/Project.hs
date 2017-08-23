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
    , _projectId
    , projectId
    , _projectName
    , _projectDescription
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
import           Control.Lens ((^.), makeLenses, to)
import           Data.Monoid ((<>))
import           Data.Profunctor.Product (p2)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Text (Text)
import           Database.PostgreSQL.Simple (Connection)
import           Opaleye (Column, PGInt4, PGText, Query, Table(Table), (.===), (.==), optional, pgInt4, pgStrictText, queryTable, required, restrict, runDelete, runInsertMany, runInsertManyReturning, runQuery, runUpdate)
import           Text.Blaze (ToValue, toValue)

import           OpaleyeUtils (runFindQuery, withId)
import           User (UserColumnRead, UserId, UserIdColumn, userId, userIdColumn, userQuery)

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
    { _projectId :: a
    , _projectName :: b
    , _projectDescription :: c
    } deriving (Eq, Show)
type Project = Project' ProjectId Text Text
type ProjectColumnWrite = Project' ProjectIdColumnMaybe (Column PGText) (Column PGText)
type ProjectColumnRead = Project' ProjectIdColumn (Column PGText) (Column PGText)
$(makeAdaptorAndInstance "pProject" ''Project')
makeLenses ''Project'

instance ToValue a => ToValue (Project' a b c) where
    toValue = toValue . _projectId

projectsTable :: Table ProjectColumnWrite ProjectColumnRead
projectsTable = Table "projects"
                      (pProject Project { _projectId = pProjectId (ProjectId (optional "id"))
                                        , _projectName = required "name"
                                        , _projectDescription = required "description"
                                        })

projectQuery :: Query ProjectColumnRead
projectQuery = queryTable projectsTable

projectsUsersTable :: Table (ProjectIdColumn, UserIdColumn) (ProjectIdColumn, UserIdColumn)
projectsUsersTable = Table "projects_users"
                           (p2 ( pProjectId (ProjectId (required "project_id"))
                               , userIdColumn (required "user_id")))

projectsUsersQuery :: Query (ProjectColumnRead, UserColumnRead)
projectsUsersQuery = proc () -> do
    user <- userQuery -< ()
    project <- projectQuery -< ()
    (puProjectId, puUserId) <- queryTable projectsUsersTable -< ()

    restrict -< project^.projectId .=== puProjectId
    restrict -< user^.userId .=== puUserId

    returnA -< (project, user)

create :: Connection -> Text -> Text -> IO Project
create conn name' description' = do
    [project] <- runInsertManyReturning conn projectsTable [Project (ProjectId Nothing) (pgStrictText name') (pgStrictText description')] id
    return project

addUser :: Connection -> ProjectId -> UserId -> IO ()
addUser conn projectId' userId' = do
    _ <- runInsertMany conn projectsUsersTable [(pgInt4 <$> projectId', pgInt4 <$> userId')]
    return ()

find :: Connection -> ProjectId -> IO (Maybe Project)
find conn projectId' = do
    runFindQuery conn (findQuery projectId')

findQuery :: ProjectId -> Query ProjectColumnRead
findQuery projectId' = proc () -> do
    project <- projectQuery -< ()
    withId projectId' -< _projectId project
    returnA -< project

findByName :: Connection -> Text -> IO (Maybe Project)
findByName conn projectName' = do
    runFindQuery conn (findByNameQuery projectName')

findByNameQuery :: Text -> Query ProjectColumnRead
findByNameQuery projectName' = proc () -> do
    project <- projectQuery -< ()
    restrict -< project^.projectName .== pgStrictText projectName'
    returnA -< project

findByUserId :: Connection -> UserId -> ProjectId -> IO (Maybe Project)
findByUserId conn userId' projectId' = do
    runFindQuery conn (findByUserIdQuery userId' projectId')

findByUserIdQuery :: UserId -> ProjectId -> Query ProjectColumnRead
findByUserIdQuery userId' projectId' = proc () -> do
    (project, user) <- projectsUsersQuery -< ()

    withId userId' -< user^.userId
    withId projectId' -< project^.projectId

    returnA -< project

allByUserId :: Connection -> UserId -> IO [Project]
allByUserId conn userId' =
    runQuery conn (allByUserIdQuery userId')

allByUserIdQuery :: UserId -> Query ProjectColumnRead
allByUserIdQuery userId' = proc () -> do
    (project, user) <- projectsUsersQuery -< ()

    withId userId' -< user^.userId

    returnA -< project

update :: Connection -> Project -> IO ()
update conn project = do
    _ <- runUpdate
             conn
             projectsTable
             (\p -> p { _projectId = Just <$> p^.projectId
                      , _projectName = project^.projectName.to pgStrictText
                      , _projectDescription = project^.projectDescription.to pgStrictText
                      })
             (\p -> p^.projectId .=== (pgInt4 <$> project^.projectId))
    return ()

delete :: Connection -> ProjectId -> IO ()
delete conn projectId' = do
    _ <- runDelete conn projectsTable (\p -> p^.projectId .=== (pgInt4 <$> projectId'))
    return ()
