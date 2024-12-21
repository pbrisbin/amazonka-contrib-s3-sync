module Amazonka.S3.Sync.Source
  ( sourceLocalRemote
  , sourceRemoteLocal
  , sourceRemoteRemote
  ) where

import Amazonka.S3.Sync.Prelude

import Amazonka.S3.Sync.CompareKey
import Amazonka.S3.Sync.FileDetails
import Amazonka.S3.Sync.Item
import Amazonka.S3.Sync.Key as Key
import Amazonka.S3.Sync.Logic
import Amazonka.S3.Sync.ObjectAttributes
import Conduit
import Control.Monad.Directory
import Path

sourceLocalRemote
  :: (MonadThrow m, MonadDirectory m, MonadAWS m)
  => Path Abs Dir
  -> BucketKey Abs Prefix
  -> ConduitT i (These (SyncItem Path File) (SyncItem Key Object)) m ()
sourceLocalRemote = runSyncLogic localRemoteLogic

sourceRemoteLocal
  :: (MonadThrow m, MonadDirectory m, MonadAWS m)
  => BucketKey Abs Prefix
  -> Path Abs Dir
  -> ConduitT i (These (SyncItem Key Object) (SyncItem Path File)) m ()
sourceRemoteLocal = runSyncLogic remoteLocalLogic

sourceRemoteRemote
  :: (MonadThrow m, MonadAWS m)
  => BucketKey Abs Prefix
  -> BucketKey Abs Prefix
  -> ConduitT i (These (SyncItem Key Object) (SyncItem Key Object)) m ()
sourceRemoteRemote = runSyncLogic remoteRemoteLogic

localRemoteLogic
  :: (MonadThrow m, MonadDirectory m, MonadAWS m)
  => SyncLogic
      m
      (Path Abs Dir)
      (Path Abs File, FileDetails)
      (SyncItem Path File)
      (BucketKey Abs Prefix)
      (Key Abs Object, ObjectAttributes)
      (SyncItem Key Object)
localRemoteLogic =
  SyncLogic
    { listSource = listDirWithFileDetails
    , listTarget = listPrefixWithObjectAttributes
    , toSourceItem = syncItemFile
    , toTargetItem = syncItemObject
    , compareDir = compareDirPrefix
    , compareItem = compareSyncItems
    }

remoteLocalLogic
  :: (MonadThrow m, MonadDirectory m, MonadAWS m)
  => SyncLogic
      m
      (BucketKey Abs Prefix)
      (Key Abs Object, ObjectAttributes)
      (SyncItem Key Object)
      (Path Abs Dir)
      (Path Abs File, FileDetails)
      (SyncItem Path File)
remoteLocalLogic =
  SyncLogic
    { listSource = listPrefixWithObjectAttributes
    , listTarget = listDirWithFileDetails
    , toSourceItem = syncItemObject
    , toTargetItem = syncItemFile
    , compareDir = comparePrefixDir
    , compareItem = compareSyncItems
    }

remoteRemoteLogic
  :: (MonadThrow m, MonadAWS m)
  => SyncLogic
      m
      (BucketKey Abs Prefix)
      (Key Abs Object, ObjectAttributes)
      (SyncItem Key Object)
      (BucketKey Abs Prefix)
      (Key Abs Object, ObjectAttributes)
      (SyncItem Key Object)
remoteRemoteLogic =
  SyncLogic
    { listSource = listPrefixWithObjectAttributes
    , listTarget = listPrefixWithObjectAttributes
    , toSourceItem = syncItemObject
    , toTargetItem = syncItemObject
    , compareDir = comparePrefixPrefix
    , compareItem = compareSyncItems
    }

compareDirPrefix
  :: MonadThrow m
  => Path Abs Dir
  -> BucketKey Abs Prefix
  -> Path Abs Dir
  -> BucketKey Abs Prefix
  -> m Ordering
compareDirPrefix source target s t = do
  compareOnKey
    <$> Path.stripProperPrefix source s
    <*> Key.stripProperPrefix target.key t.key

comparePrefixDir
  :: MonadThrow m
  => BucketKey Abs Prefix
  -> Path Abs Dir
  -> BucketKey Abs Prefix
  -> Path Abs Dir
  -> m Ordering
comparePrefixDir source target s t = do
  compareOnKey
    <$> Key.stripProperPrefix source.key s.key
    <*> Path.stripProperPrefix target t

comparePrefixPrefix
  :: MonadThrow m
  => BucketKey Abs Prefix
  -> BucketKey Abs Prefix
  -> BucketKey Abs Prefix
  -> BucketKey Abs Prefix
  -> m Ordering
comparePrefixPrefix source target s t = do
  compareOnKey
    <$> Key.stripProperPrefix source.key s.key
    <*> Key.stripProperPrefix target.key t.key
