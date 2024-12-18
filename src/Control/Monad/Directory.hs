module Control.Monad.Directory
  ( MonadDirectory (..)
  ) where

import Prelude

import Data.Time (UTCTime)
import Path

class Monad m => MonadDirectory m where
  listDir :: Path Abs Dir -> m ([Path Abs Dir], [Path Abs File])
  doesFileExist :: Path Abs File -> m Bool
  getFileSize :: Path Abs File -> m Integer
  getModificationTime :: Path Abs File -> m UTCTime
