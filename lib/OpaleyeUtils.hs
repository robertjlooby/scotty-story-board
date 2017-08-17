{-# LANGUAGE Arrows                     #-}
{-# LANGUAGE FlexibleContexts           #-}

module OpaleyeUtils where

import           Data.Profunctor.Product.Default (Default)
import           Opaleye (Column, PGInt4, QueryArr, (.===), pgInt4, restrict)
import           Opaleye.Internal.Operators (EqPP)

withId :: (Functor f, Default EqPP (f (Column PGInt4)) (f (Column PGInt4))) => f Int -> QueryArr (f (Column PGInt4)) ()
withId id' = proc id'' -> do
    restrict -< (pgInt4 <$> id') .=== id''
