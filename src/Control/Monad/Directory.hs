module Control.Monad.Directory
  ( MonadDirectory (..)
  ) where

import Prelude

import Conduit
import Data.Time (UTCTime)
import Path

class Monad m => MonadDirectory m where
  listDirRel :: Path Rel Dir -> m ([Path Rel Dir], [Path Rel File])
  doesFileExist :: Path Rel File -> m Bool
  getFileSize :: Path Rel File -> m Integer
  getModificationTime :: Path Rel File -> m UTCTime

instance MonadDirectory m => MonadDirectory (ConduitT i o m) where
  listDirRel = lift . listDirRel
  doesFileExist = lift . doesFileExist
  getFileSize = lift . getFileSize
  getModificationTime = lift . getModificationTime
