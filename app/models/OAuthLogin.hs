{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module OAuthLogin
    (
    -- * Queries
      create
    , findUserQuery
    ) where

import           Control.Arrow (returnA)
import           Control.Lens ((^.))
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Text (Text)
import           Database.PostgreSQL.Simple (Connection)
import           Opaleye (Column, PGText, Query, Table(Table), (.===), (.==), pgInt4, pgStrictText, queryTable, required, restrict, runInsertMany)

import           User (UserColumnRead, UserId, UserIdColumn, userId, userIdColumn, userQuery)

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
create conn userId' providerName providerUserId = do
    _ <- runInsertMany
             conn
             oAuthLoginsTable
             [OAuthLogin (pgInt4 <$> userId') (pgStrictText providerName) (pgStrictText providerUserId)]
    return ()

findUserQuery :: Text -> Text -> Query UserColumnRead
findUserQuery providerName providerUserId = proc () -> do
    user <- userQuery -< ()
    oAuthLogin <- oAuthLoginQuery -< ()

    restrict -< oalUserId oAuthLogin .=== user^.userId
    restrict -< oalProviderName oAuthLogin .== pgStrictText providerName
    restrict -< oalProviderUserId oAuthLogin .== pgStrictText providerUserId

    returnA -< user
