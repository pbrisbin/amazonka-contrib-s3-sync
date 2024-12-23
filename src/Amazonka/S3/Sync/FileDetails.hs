{-# LANGUAGE TupleSections #-}

module Amazonka.S3.Sync.FileDetails
  ( FileDetails (..)
  , listDirWithFileDetails
  ) where

import Amazonka.S3.Sync.Prelude

import Control.Monad.Directory

data FileDetails = FileDetails
  { size :: Integer
  , mtime :: UTCTime
  }
  deriving stock (Eq, Show)

listDirWithFileDetails
  :: MonadDirectory m
  => Path Rel Dir
  -> m ([Path Rel Dir], [(Path Rel File, FileDetails)])
listDirWithFileDetails =
  bimapM pure (traverse $ \f -> (f,) <$> getFileDetails f) <=< listDirRel

getFileDetails :: MonadDirectory m => Path Rel File -> m FileDetails
getFileDetails p = do
  FileDetails
    <$> getFileSize p
    <*> getModificationTime p
