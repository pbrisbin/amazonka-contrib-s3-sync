module Control.Monad.PathsRef
  ( PathsRef (..)
  , MonadPathsRef (..)
  ) where

import Amazonka.S3.Sync.Prelude

import Data.Set (Set)
import UnliftIO.IORef (IORef)

newtype PathsRef = PathsRef
  { unwrap :: IORef (Set (Path Abs File))
  }

class Monad m => MonadPathsRef m where
  getPathsRef :: m PathsRef
  recordSeen :: PathsRef -> Path Abs File -> m ()
  wasn'tSeen :: PathsRef -> Path Abs File -> m Bool
