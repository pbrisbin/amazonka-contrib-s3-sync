module Amazonka.S3.Sync.Folder
  ( SyncFolder (..)
  , withSyncDir
  , withSyncPrefix
  ) where

import Amazonka.S3.Sync.Prelude

import Amazonka.S3.Sync.Key

newtype SyncFolder = SyncFolder
  { unwrap :: Either (Path Abs Dir) (BucketKey Abs Prefix)
  }
  deriving stock (Eq, Show)

instance ToText SyncFolder where
  toText = either toText toText . (.unwrap)

withSyncDir :: MonadThrow m => (Path Abs Dir -> m a) -> SyncFolder -> m a
withSyncDir f = either f (\_ -> throwM $ userError "TODO") . (.unwrap)

withSyncPrefix
  :: MonadThrow m => (BucketKey Abs Prefix -> m a) -> SyncFolder -> m a
withSyncPrefix f = either (\_ -> throwM $ userError "TODO") f . (.unwrap)
