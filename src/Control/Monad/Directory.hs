module Control.Monad.Directory
  ( MonadDirectory (..)
  ) where

import Prelude

import Data.Time (UTCTime)
import Path

class Monad m => MonadDirectory m where
  listDir :: Path b Dir -> m ([Path Abs Dir], [Path Abs File])
  doesFileExist :: Path b File -> m Bool
  getFileSize :: Path b File -> m Integer
  getModificationTime :: Path b File -> m UTCTime
