{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module User
    (
    -- * Types
      User
    , UserId(..)
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

newtype UserId = UserId Int deriving (Eq, FromField, FromJSON, Ord, Show, ToField, ToJSON)

instance FromRow UserId where
    fromRow = UserId <$> field

instance ToValue UserId where
    toValue (UserId userId) = "/users/" <> toValue userId

data User = User
    { id_ :: UserId
    , name :: Text
    , email :: Text
    } deriving (Eq, Show)

instance FromRow User where
    fromRow = User <$> field <*> field <*> field

instance ToValue User where
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
