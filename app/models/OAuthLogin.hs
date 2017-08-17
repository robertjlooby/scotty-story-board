{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module OAuthLogin
    (
    -- * Queries
      create
    , findUser
    ) where

import           Control.Arrow (returnA)
import           Data.Maybe (listToMaybe)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Text (Text)
import           Database.PostgreSQL.Simple (Connection)
import           Opaleye (Column, PGText, Query, Table(Table), (.===), (.==), pgInt4, pgStrictText, queryTable, required, restrict, runInsertMany)

import           User (User, UserColumnRead, UserId, UserIdColumn, runUserQuery, userIdColumn, userQuery)
import qualified User

data OAuthLogin' a b c = OAuthLogin
    { oalUserId :: a
    , oalProviderName :: b
    , oalProviderUserId :: c
    } deriving (Eq, Show)
type OAuthLoginColumn = OAuthLogin' UserIdColumn (Column PGText) (Column PGText)
$(makeAdaptorAndInstance "pOAuthLogin" ''OAuthLogin')

oAuthLoginsTable :: Table OAuthLoginColumn OAuthLoginColumn
oAuthLoginsTable = Table "oauth_logins"
                      (pOAuthLogin OAuthLogin { oalUserId = userIdColumn (required "user_id")
                                              , oalProviderName = required "provider_name"
                                              , oalProviderUserId = required "provider_user_id"
                                              })

oAuthLoginQuery :: Query OAuthLoginColumn
oAuthLoginQuery = queryTable oAuthLoginsTable

create :: Connection -> UserId -> Text -> Text -> IO ()
create conn userId providerName providerUserId = do
    _ <- runInsertMany conn oAuthLoginsTable [OAuthLogin (pgInt4 <$> userId) (pgStrictText providerName) (pgStrictText providerUserId)]
    return ()

findUser :: Connection -> Text -> Text -> IO (Maybe User)
findUser conn providerName providerUserId = do
    listToMaybe <$> runUserQuery conn (findUserQuery providerName providerUserId)

findUserQuery :: Text -> Text -> Query UserColumnRead
findUserQuery providerName providerUserId = proc () -> do
    user <- userQuery -< ()
    oAuthLogin <- oAuthLoginQuery -< ()

    restrict -< oalUserId oAuthLogin .=== User.id_ user
    restrict -< oalProviderName oAuthLogin .== pgStrictText providerName
    restrict -< oalProviderUserId oAuthLogin .== pgStrictText providerUserId

    returnA -< user
