module Amazonka.S3.Sync.Source
  ( sourceLocalRemote
  , sourceRemoteLocal
  , sourceRemoteRemote
  ) where

import Amazonka.S3.Sync.Prelude

import Amazonka.S3.Sync.FileDetails
import Amazonka.S3.Sync.Item
import Amazonka.S3.Sync.Key
import Amazonka.S3.Sync.Logic
import Amazonka.S3.Sync.ObjectAttributes
import Conduit
import Control.Monad.Directory

sourceLocalRemote
  :: (MonadThrow m, MonadDirectory m, MonadAWS m)
  => Path Abs Dir
  -> BucketKey Abs Prefix
  -> ConduitT i (These (SyncItem Path File) (SyncItem Key Object)) m ()
sourceLocalRemote = runSyncLogic localRemoteLogic

sourceRemoteLocal
  :: Monad m
  => BucketKey Abs Prefix
  -> Path Abs Dir
  -> ConduitT i (These (SyncItem Key Object) (SyncItem Path File)) m ()
sourceRemoteLocal = runSyncLogic remoteLocalLogic

sourceRemoteRemote
  :: Monad m
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
    , compareDir = undefined
    , compareItem = compareSyncItems
    }

remoteLocalLogic
  :: SyncLogic
      m
      (BucketKey Abs Prefix)
      (Key Abs Object, ObjectAttributes)
      (SyncItem Key Object)
      (Path Abs Dir)
      (Path Abs File, FileDetails)
      (SyncItem Path File)
remoteLocalLogic = undefined

remoteRemoteLogic
  :: SyncLogic
      m
      (BucketKey Abs Prefix)
      (Key Abs Object, ObjectAttributes)
      (SyncItem Key Object)
      (BucketKey Abs Prefix)
      (Key Abs Object, ObjectAttributes)
      (SyncItem Key Object)
remoteRemoteLogic = undefined
