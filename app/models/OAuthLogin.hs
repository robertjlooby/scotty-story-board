{-# LANGUAGE OverloadedStrings #-}

module OAuthLogin where

import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection, execute, query)
import User (User, UserId)

create :: Connection -> UserId -> Text -> Text -> IO ()
create conn userId providerName providerUserId = do
    _ <- execute conn "INSERT INTO oauth_logins (user_id, provider_name, provider_user_id) VALUES (?, ?, ?)" (userId, providerName, providerUserId)
    return ()

findUser :: Connection -> Text -> Text -> IO (Maybe User)
findUser conn providerName providerUserId = do
    users <- query
               conn
               "SELECT users.id, name FROM users INNER JOIN oauth_logins ON users.id = oauth_logins.user_id WHERE provider_name = ? AND provider_user_id = ?"
               (providerName, providerUserId)
    case users of
        [user] -> return $ Just user
        _      -> return Nothing
