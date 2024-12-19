{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}

module Amazonka.S3.Sync.Item
  ( SyncItem (..)
  , syncItemFile
  , syncItemObject
  , compareSyncItems
  ) where

import Amazonka.S3.Sync.Prelude

import Amazonka.S3.Sync.FileDetails
import Amazonka.S3.Sync.Key as Key
import Amazonka.S3.Sync.ObjectAttributes
import Path
import qualified Path.Posix as Posix

data SyncItem (k :: Type -> Type -> Type) (t :: Type) = SyncItem
  { location :: k Rel t
  , size :: Integer
  , mtime :: UTCTime
  }

deriving stock instance Eq (k Rel t) => Eq (SyncItem k t)
deriving stock instance Ord (k Rel t) => Ord (SyncItem k t)

instance ToText (k Rel t) => ToText (SyncItem k t) where
  toText = toText . (.location)

syncItemFile
  :: MonadThrow m
  => Path Abs Dir
  -> (Path Abs File, FileDetails)
  -> m (SyncItem Path File)
syncItemFile dir (f, fd) = do
  location <- Path.stripProperPrefix dir f
  pure $ SyncItem {location, size = fd.size, mtime = fd.mtime}

syncItemObject
  :: MonadThrow m
  => BucketKey Abs Prefix
  -> (Key Abs Object, ObjectAttributes)
  -> m (SyncItem Key Object)
syncItemObject bk (o, oa) = do
  location <- Key.stripProperPrefix bk.key o
  pure $ SyncItem {location, size = oa.size, mtime = oa.lastModified}

class ToCompareKey a where
  toCompareKey :: a -> Text

instance ToCompareKey (SyncItem Path t) where
  toCompareKey = pack . Posix.toFilePath . (.location)

instance ToCompareKey (SyncItem Key t) where
  toCompareKey = toText . toObjectKey . (.location)

compareSyncItems
  :: ( ToCompareKey (SyncItem k1 t1)
     , ToCompareKey (SyncItem k2 t2)
     )
  => SyncItem k1 t1
  -> SyncItem k2 t2
  -> Ordering
compareSyncItems a b = toCompareKey a `compare` toCompareKey b
