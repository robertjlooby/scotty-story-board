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
    , id_
    , name
    , email
    -- * Queries
    , userQuery
    , runUserQuery
    , create
    , find
    , findByName
    , update
    ) where

import           Control.Arrow (returnA)
import           Data.Aeson (FromJSON, ToJSON)
import           Data.Monoid ((<>))
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Text (Text)
import           Database.PostgreSQL.Simple (Connection)
import           Opaleye (Column, PGInt4, PGText, Query, Table(Table), TableProperties, (.===), (.==), optional, pgInt4, pgStrictText, queryTable, required, restrict, runInsertManyReturning, runQuery, runUpdate)
import           Text.Blaze (ToValue, toValue)

newtype UserId' a = UserId a deriving (Eq, FromJSON, Ord, Show, ToJSON)
type UserId = UserId' Int
type UserIdColumn = UserId' (Column PGInt4)
type UserIdColumnMaybe = UserId' (Maybe (Column PGInt4))
$(makeAdaptorAndInstance "pUserId" ''UserId')

userIdColumn :: TableProperties a b -> TableProperties (UserId' a) (UserId' b)
userIdColumn tableProperties = pUserId (UserId tableProperties)

instance Functor UserId' where
    fmap f (UserId a) = UserId (f a)

instance ToValue a => ToValue (UserId' a) where
    toValue (UserId userId) = "/users/" <> toValue userId

data User' a b c = User
    { id_ :: a
    , name :: b
    , email :: c
    } deriving (Eq, Show)
type User = User' UserId Text Text
type UserColumnWrite = User' UserIdColumnMaybe (Column PGText) (Column PGText)
type UserColumnRead = User' UserIdColumn (Column PGText) (Column PGText)
$(makeAdaptorAndInstance "pUser" ''User')

usersTable :: Table UserColumnWrite UserColumnRead
usersTable = Table "users"
                  (pUser User { id_ = userIdColumn (optional "id")
                              , name = required "name"
                              , email = required "email"
                              })

userQuery :: Query UserColumnRead
userQuery = queryTable usersTable

runUserQuery :: Connection -> Query UserColumnRead -> IO [User]
runUserQuery = runQuery

instance ToValue a => ToValue (User' a b c) where
    toValue = toValue . id_

create :: Connection -> Text -> Text -> IO User
create conn name' email' = do
    [user] <- runInsertManyReturning conn usersTable [User (UserId Nothing) (pgStrictText name') (pgStrictText email')] id
    return user

find :: Connection -> UserId -> IO (Maybe User)
find conn userId = do
    users <- runUserQuery conn (findQuery userId)
    case users of
        [user] -> return $ Just user
        _      -> return Nothing

findQuery :: UserId -> Query UserColumnRead
findQuery userId = proc () -> do
    row <- userQuery -< ()
    restrict -< id_ row .=== (pgInt4 <$> userId)
    returnA -< row

findByName :: Connection -> Text -> IO (Maybe User)
findByName conn userName = do
    users <- runUserQuery conn (findByNameQuery userName)
    case users of
        [user] -> return $ Just user
        _      -> return Nothing

findByNameQuery :: Text -> Query UserColumnRead
findByNameQuery userName = proc () -> do
    row <- userQuery -< ()
    restrict -< name row .== pgStrictText userName
    returnA -< row

update :: Connection -> User -> IO ()
update conn user = do
    _ <- runUpdate conn usersTable (\u -> u {id_ = Just <$> id_ u, name = pgStrictText (name user), email = pgStrictText (email user)}) (\u -> id_ u .=== (pgInt4 <$> id_ user))
    return ()
