module Control.Monad.Directory
  ( MonadDirectory (..)
  ) where

import Prelude

import Data.Time (UTCTime)
import Path

class Monad m => MonadDirectory m where
  listDirectory :: Path b Dir -> m ([Path Rel Dir], [Path Rel File])
  doesDirectoryExist :: Path b Dir -> m Bool
  doesFileExist :: Path b File -> m Bool
  getFileSize :: Path b File -> m Integer
  getModificationTime :: Path b File -> m UTCTime
