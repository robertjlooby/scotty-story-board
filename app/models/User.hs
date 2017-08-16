{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module User
    (
    -- * Types
      User
    , UserId'(..)
    , UserId
    -- * Accessors
    , id_
    , name
    , email
    -- * Queries
    , create
    , find
    , findByName
    , update
    ) where

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import           Database.PostgreSQL.Simple (Connection, FromRow, Only(..), execute, query)
import           Database.PostgreSQL.Simple.FromField (FromField)
import           Database.PostgreSQL.Simple.FromRow (field, fromRow)
import           Database.PostgreSQL.Simple.ToField (ToField)
import           Text.Blaze (ToValue, toValue)

newtype UserId' a = UserId a deriving (Eq, FromField, FromJSON, Ord, Show, ToField, ToJSON)
type UserId = UserId' Int

instance FromField a => FromRow (UserId' a) where
    fromRow = UserId <$> field

instance ToValue a => ToValue (UserId' a) where
    toValue (UserId userId) = "/users/" <> toValue userId

data User' a b c = User
    { id_ :: a
    , name :: b
    , email :: c
    } deriving (Eq, Show)
type User = User' UserId Text Text

instance (FromField a, FromField b, FromField c) => FromRow (User' a b c) where
    fromRow = User <$> field <*> field <*> field

instance ToValue a => ToValue (User' a b c) where
    toValue = toValue . id_

create :: Connection -> Text -> Text -> IO User
create conn name' email' = do
    [user] <- query conn "INSERT INTO users (name, email) VALUES (?, ?) RETURNING *" (name', email')
    return user

find :: Connection -> UserId -> IO (Maybe User)
find conn (UserId userId) = do
    users <- query conn "SELECT id, name, email FROM users WHERE id = ?" $ Only userId
    case users of
        [user] -> return $ Just user
        _      -> return Nothing

findByName :: Connection -> Text -> IO (Maybe User)
findByName conn userName = do
    users <- query conn "SELECT id, name, email FROM users WHERE name = ?" $ Only userName
    case users of
        [user] -> return $ Just user
        _      -> return Nothing

update :: Connection -> User -> IO ()
update conn user = do
    _ <- execute conn "UPDATE users SET name = ?, email = ? WHERE id = ?" (name user, email user, id_ user)
    return ()
