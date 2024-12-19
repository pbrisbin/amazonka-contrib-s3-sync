{-# LANGUAGE DuplicateRecordFields #-}

module Amazonka.S3.Sync.Target
  ( SyncTargetLocal (..)
  , SyncTargetRemote (..)
  , ListTarget (..)
  ) where

import Amazonka.S3.Sync.Prelude

import Amazonka.S3.Sync.Folder
import Amazonka.S3.Sync.Item
import Amazonka.S3.Sync.Key
import Amazonka.S3.Sync.LocalFile
import Amazonka.S3.Sync.RemoteObject
import Amazonka.S3.Sync.RemotePrefix
import Amazonka.S3.Sync.SharedPath
import Control.Monad.Directory

newtype SyncTargetLocal = SyncTargetLocal
  { unwrap :: Path Abs Dir
  }
  deriving stock (Eq, Show)
  deriving newtype
    ( ListTarget m
    , ToSharedPath SyncFolder
    , ToSharedPath SyncItem
    )

newtype SyncTargetRemote = SyncTargetRemote
  { unwrap :: BucketKey Abs Prefix
  }
  deriving stock (Eq, Show)
  deriving newtype
    ( ToRemotePrefix
    , ListTarget m
    , ToSharedPath SyncFolder
    , ToSharedPath SyncItem
    )

class ListTarget m t where
  listTarget :: t -> m ([SyncFolder], [SyncItem])

instance (MonadThrow m, MonadDirectory m) => ListTarget m (Path Abs Dir) where
  listTarget = listLocalFiles

instance (MonadThrow m, MonadAWS m) => ListTarget m (BucketKey Abs Prefix) where
  listTarget = listRemoteObjects

instance (ListTarget m a, ListTarget m b) => ListTarget m (Either a b) where
  listTarget = either listTarget listTarget

instance (MonadThrow m, MonadDirectory m, MonadAWS m) => ListTarget m SyncFolder where
  listTarget = listTarget . (.unwrap)
