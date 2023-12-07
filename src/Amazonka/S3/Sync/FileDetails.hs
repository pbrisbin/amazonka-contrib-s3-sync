module Amazonka.S3.Sync.FileDetails
  ( FileDetails (..)
  , getFileDetails
  ) where

import Amazonka.S3.Sync.Prelude

import Control.Monad.Directory

data FileDetails = FileDetails
  { size :: Integer
  , mtime :: UTCTime
  }

getFileDetails :: MonadDirectory m => Path b File -> m (Maybe FileDetails)
getFileDetails p = do
  exists <- doesFileExist p

  if exists
    then
      fmap Just
        $ FileDetails
        <$> getFileSize p
        <*> getModificationTime p
    else pure Nothing
