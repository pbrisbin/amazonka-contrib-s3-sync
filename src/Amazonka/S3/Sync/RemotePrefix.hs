module Amazonka.S3.Sync.RemotePrefix
  ( ToRemotePrefix (..)
  ) where

import Amazonka.S3.Sync.Prelude

import Amazonka.S3.Sync.Key

class ToRemotePrefix a where
  toRemotePrefix :: a -> BucketKey Abs Prefix

instance ToRemotePrefix (BucketKey Abs Prefix) where
  toRemotePrefix = id
