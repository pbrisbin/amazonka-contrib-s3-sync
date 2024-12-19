module Amazonka.S3.Sync.Item
  ( SyncItem (..)
  , withSyncFile
  , withSyncObject
  ) where

import Amazonka.S3.Sync.Prelude

import Amazonka.S3.Sync.Key

data SyncItem = SyncItem
  { location :: Either (Path Abs File) (BucketKey Abs Object)
  , size :: Integer
  , mtime :: UTCTime
  }
  deriving stock (Eq, Show)

instance ToText SyncItem where
  toText = either toText toText . (.location)

withSyncFile :: MonadThrow m => (Path Abs File -> m a) -> SyncItem -> m a
withSyncFile f = either f (\_ -> throwM $ userError "TODO") . (.location)

withSyncObject
  :: MonadThrow m => (BucketKey Abs Object -> m a) -> SyncItem -> m a
withSyncObject f = either (\_ -> throwM $ userError "TODO") f . (.location)
