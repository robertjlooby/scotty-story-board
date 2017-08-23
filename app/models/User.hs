{-# LANGUAGE Arrows                     #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module User
    (
    -- * Types
      User
    , UserId'(..)
    , UserId
    , UserIdColumn
    , UserColumnRead
    , userIdColumn
    -- * Accessors
    , _userId
    , userId
    , _userName
    , userName
    , _userEmail
    , userEmail
    -- * Queries
    , runUserFindQuery
    , userQuery
    , create
    , findQuery
    , findByName
    , update
    ) where

import           Control.Arrow (returnA)
import           Control.Lens ((^.), makeLenses, to)
import           Data.Aeson (FromJSON, ToJSON)
import           Data.Monoid ((<>))
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Text (Text)
import           Database.PostgreSQL.Simple (Connection)
import           Opaleye (Column, PGInt4, PGText, Query, Table(Table), TableProperties, (.===), (.==), optional, pgInt4, pgStrictText, queryTable, required, restrict, runInsertManyReturning, runUpdate)
import           Text.Blaze (ToValue, toValue)

import           OpaleyeUtils (runFindQuery, withId)

newtype UserId' a = UserId a deriving (Eq, FromJSON, Ord, Show, ToJSON)
type UserId = UserId' Int
type UserIdColumn = UserId' (Column PGInt4)
type UserIdColumnMaybe = UserId' (Maybe (Column PGInt4))
$(makeAdaptorAndInstance "pUserId" ''UserId')

instance Functor UserId' where
    fmap f (UserId a) = UserId (f a)

instance ToValue a => ToValue (UserId' a) where
    toValue (UserId userId) = "/users/" <> toValue userId

userIdColumn :: TableProperties a b -> TableProperties (UserId' a) (UserId' b)
userIdColumn tableProperties = pUserId (UserId tableProperties)

data User' a b c = User
    { _userId :: a
    , _userName :: b
    , _userEmail :: c
    } deriving (Eq, Show)
type User = User' UserId Text Text
type UserColumnWrite = User' UserIdColumnMaybe (Column PGText) (Column PGText)
type UserColumnRead = User' UserIdColumn (Column PGText) (Column PGText)
$(makeAdaptorAndInstance "pUser" ''User')
makeLenses ''User'

instance ToValue a => ToValue (User' a b c) where
    toValue = toValue . _userId

usersTable :: Table UserColumnWrite UserColumnRead
usersTable = Table "users"
                  (pUser User { _userId = userIdColumn (optional "id")
                              , _userName = required "name"
                              , _userEmail = required "email"
                              })

userQuery :: Query UserColumnRead
userQuery = queryTable usersTable

runUserFindQuery :: Connection -> Query UserColumnRead -> IO (Maybe User)
runUserFindQuery = runFindQuery

create :: Connection -> Text -> Text -> IO User
create conn name' email' = do
    [user] <- runInsertManyReturning conn usersTable [User (UserId Nothing) (pgStrictText name') (pgStrictText email')] id
    return user

findQuery :: UserId -> Query UserColumnRead
findQuery userId' = proc () -> do
    user <- userQuery -< ()
    withId userId' -< user^.userId
    returnA -< user

findByName :: Connection -> Text -> IO (Maybe User)
findByName conn userName' = do
    runFindQuery conn (findByNameQuery userName')

findByNameQuery :: Text -> Query UserColumnRead
findByNameQuery userName' = proc () -> do
    user <- userQuery -< ()
    restrict -< (user^.userName) .== (userName'^.to pgStrictText)
    returnA -< user

update :: Connection -> User -> IO ()
update conn user = do
    _ <- runUpdate
             conn
             usersTable
             (\u -> u {_userId = Just <$> u^.userId, _userName =  user^.userName.to pgStrictText, _userEmail = user^.userEmail.to pgStrictText})
             (\u -> u^.userId .=== (pgInt4 <$> user^.userId))
    return ()
