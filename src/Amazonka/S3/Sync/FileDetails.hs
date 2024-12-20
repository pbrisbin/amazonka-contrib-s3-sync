{-# LANGUAGE TupleSections #-}

module Amazonka.S3.Sync.FileDetails
  ( FileDetails (..)
  , getFileDetails
  , listDirWithFileDetails
  ) where

import Amazonka.S3.Sync.Prelude

import Control.Monad.Directory

data FileDetails = FileDetails
  { size :: Integer
  , mtime :: UTCTime
  }
  deriving stock (Eq, Show)

getFileDetails :: MonadDirectory m => Path Abs File -> m FileDetails
getFileDetails p = do
  FileDetails
    <$> getFileSize p
    <*> getModificationTime p

listDirWithFileDetails
  :: MonadDirectory m
  => Path Abs Dir
  -> m ([Path Abs Dir], [(Path Abs File, FileDetails)])
listDirWithFileDetails =
  bimapM pure (traverse $ \f -> (f,) <$> getFileDetails f) <=< listDir
