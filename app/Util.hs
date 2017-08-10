{-# LANGUAGE OverloadedStrings #-}

module Util
    ( logError
    ) where

import           Control.Logging (LogLevel(LevelError), loggingLogger)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           System.Log.FastLogger (ToLogStr)

logError :: (ToLogStr a, MonadIO m) => a -> m ()
logError = liftIO <$> loggingLogger LevelError ""
