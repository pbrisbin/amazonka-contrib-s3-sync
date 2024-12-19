module Amazonka.S3.Sync.LocalFile
  ( listLocalFiles
  ) where

import Amazonka.S3.Sync.Prelude

import Amazonka.S3.Sync.FileDetails
import Amazonka.S3.Sync.Folder
import Amazonka.S3.Sync.Item
import Control.Monad.Directory

listLocalFiles
  :: (MonadThrow m, MonadDirectory m)
  => Path Abs Dir
  -> m ([SyncFolder], [SyncItem])
listLocalFiles d = do
  contents <- listDir d
  bimapM
    (pure . map (SyncFolder . Left))
    ( traverse $ \f -> do
        details <- getFileDetails f
        pure
          SyncItem
            { location = Left f
            , size = details.size
            , mtime = details.mtime
            }
    )
    contents
