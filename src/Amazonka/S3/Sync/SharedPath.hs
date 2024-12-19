module Amazonka.S3.Sync.SharedPath
  ( SharedPath (..)
  , ToSharedPath (..)
  , FromSharedPath (..)
  ) where

import Amazonka.S3.Sync.Prelude

import Amazonka.S3.Sync.Folder
import Amazonka.S3.Sync.Item
import Amazonka.S3.Sync.Key as Key
import Path
import qualified Path.Posix as Posix

-- | A 'SharedPath' is relative and formatted the same between local/remote
--
-- This can be used to compare a local and remote item, or map between them.
newtype SharedPath = SharedPath
  { unwrap :: Text
  }
  deriving newtype (Show, Eq, Ord)

class ToSharedPath a s where
  toSharedPath :: MonadThrow m => s -> a -> m SharedPath

instance ToSharedPath (Path Abs t) (Path Abs Dir) where
  toSharedPath s =
    fmap (SharedPath . pack . Posix.toFilePath)
      . Path.stripProperPrefix s

instance ToSharedPath SyncFolder (Path Abs Dir) where
  toSharedPath s = withSyncDir $ toSharedPath s

instance ToSharedPath SyncItem (Path Abs Dir) where
  toSharedPath s = withSyncFile $ toSharedPath s

instance ToSharedPath (BucketKey Abs t) (BucketKey Abs Prefix) where
  toSharedPath s =
    fmap (SharedPath . toText . toObjectKey)
      . Key.stripProperPrefix s.key
      . (.key)

instance ToSharedPath SyncFolder (BucketKey Abs Prefix) where
  toSharedPath s = withSyncPrefix $ toSharedPath s

instance ToSharedPath SyncItem (BucketKey Abs Prefix) where
  toSharedPath s = withSyncObject $ toSharedPath s

class FromSharedPath a t where
  fromSharedPath :: MonadThrow m => t -> SharedPath -> m a

-- instance FromSharedPath SyncTarget (BucketKey Abs Object) where
--   fromSharedPath t p =
--     joinBucketKey t.unwrap
--       <$> parseRelObject (ObjectKey p.unwrap)
