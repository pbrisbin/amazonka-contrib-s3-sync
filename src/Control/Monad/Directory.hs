module Control.Monad.Directory
  ( MonadDirectory (..)
  ) where

import Prelude

import Conduit
import Data.Time (UTCTime)
import Path

class Monad m => MonadDirectory m where
  listDir :: Path b Dir -> m ([Path Abs Dir], [Path Abs File])
  listDirRel :: Path b Dir -> m ([Path Rel Dir], [Path Rel File])
  doesFileExist :: Path b File -> m Bool
  getFileSize :: Path b File -> m Integer
  getModificationTime :: Path b File -> m UTCTime

instance MonadDirectory m => MonadDirectory (ConduitT i o m) where
  listDir = lift . listDir
  listDirRel = lift . listDirRel
  doesFileExist = lift . doesFileExist
  getFileSize = lift . getFileSize
  getModificationTime = lift . getModificationTime
