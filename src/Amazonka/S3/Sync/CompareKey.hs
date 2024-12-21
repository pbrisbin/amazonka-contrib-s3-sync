module Amazonka.S3.Sync.CompareKey
  ( ToCompareKey (..)
  , compareOnKey
  ) where

import Amazonka.S3.Sync.Prelude

import Amazonka.S3.Sync.Key as Key
import Path

import qualified Path.Posix as Posix

class ToCompareKey a where
  toCompareKey :: a -> Text

instance ToCompareKey (Path Rel t) where
  toCompareKey = pack . Posix.toFilePath

instance ToCompareKey (Key Rel t) where
  toCompareKey = toText . toObjectKey

instance ToCompareKey (BucketKey Rel t) where
  toCompareKey = toText . toObjectKey . (.key)

compareOnKey :: (ToCompareKey a, ToCompareKey b) => a -> b -> Ordering
compareOnKey a b = toCompareKey a `compare` toCompareKey b
