{-# LANGUAGE DuplicateRecordFields #-}

module Amazonka.S3.Sync.Source
  ( SyncSourceLocal (..)
  , SyncSourceRemote (..)
  , SyncItem (..)
  , ListSource (..)
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

newtype SyncSourceLocal = SyncSourceLocal
  { unwrap :: Path Abs Dir
  }
  deriving stock (Eq, Show)
  deriving newtype
    ( ListSource m
    , ToSharedPath SyncFolder
    , ToSharedPath SyncItem
    )

newtype SyncSourceRemote = SyncSourceRemote
  { unwrap :: BucketKey Abs Prefix
  }
  deriving stock (Eq, Show)
  deriving newtype
    ( ToRemotePrefix
    , ListSource m
    , ToSharedPath SyncFolder
    , ToSharedPath SyncItem
    )

class ListSource m s where
  listSource :: s -> m ([SyncFolder], [SyncItem])

instance (MonadThrow m, MonadDirectory m) => ListSource m (Path Abs Dir) where
  listSource = listLocalFiles

instance (MonadThrow m, MonadAWS m) => ListSource m (BucketKey Abs Prefix) where
  listSource = listRemoteObjects

instance (ListSource m a, ListSource m b) => ListSource m (Either a b) where
  listSource = either listSource listSource

instance (MonadThrow m, MonadDirectory m, MonadAWS m) => ListSource m SyncFolder where
  listSource = listSource . (.unwrap)
