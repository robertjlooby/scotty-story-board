{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module User where

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Text (Text)
import           Database.PostgreSQL.Simple (Connection, FromRow, Only(..), query)
import           Database.PostgreSQL.Simple.FromField (FromField)
import           Database.PostgreSQL.Simple.FromRow (field, fromRow)
import           Database.PostgreSQL.Simple.ToField (ToField)

newtype UserId = UserId Int deriving (Eq, FromField, FromJSON, Ord, Show, ToField, ToJSON)

instance FromRow UserId where
    fromRow = UserId <$> field

data User = User
    { id_ :: UserId
    , name :: Text
    } deriving (Eq, Show)

instance FromRow User where
    fromRow = User <$> field <*> field

create :: Connection -> Text -> IO UserId
create conn name' = do
    [userId] <- query conn "INSERT INTO users (name) VALUES (?) RETURNING id" (Only name')
    return userId

find :: Connection -> UserId -> IO (Maybe User)
find conn (UserId userId) = do
    users <- query conn "SELECT id, name FROM users WHERE id = ?" $ Only userId
    case users of
        [user] -> return $ Just user
        _      -> return Nothing

findByName :: Connection -> Text -> IO (Maybe User)
findByName conn userName = do
    users <- query conn "SELECT id, name FROM users WHERE name = ?" $ Only userName
    case users of
        [user] -> return $ Just user
        _      -> return Nothing
