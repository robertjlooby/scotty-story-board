{-# LANGUAGE Arrows                     #-}
{-# LANGUAGE FlexibleContexts           #-}

module OpaleyeUtils where

import           Data.Maybe (listToMaybe)
import           Data.Profunctor.Product.Default (Default)
import           Database.PostgreSQL.Simple (Connection)
import           Opaleye (Column, PGInt4, Query, QueryArr, QueryRunner, (.===), pgInt4, restrict, runQuery)
import           Opaleye.Internal.Operators (EqPP)

withId :: (Functor f, Default EqPP (f (Column PGInt4)) (f (Column PGInt4))) => f Int -> QueryArr (f (Column PGInt4)) ()
withId id' = proc id'' -> do
    restrict -< (pgInt4 <$> id') .=== id''

runFindQuery :: Default QueryRunner columns haskell => Connection -> Query columns -> IO (Maybe haskell)
runFindQuery conn query = listToMaybe <$> runQuery conn query
