module Amazonka.S3.Sync.Options
  ( SyncOptions (..)
  , DryRun (..)
  , Delete (..)
  , SizeOnly (..)
  , SyncArguments (..)
  ) where

import Amazonka.S3.Sync.Prelude

import Amazonka.S3.Sync.Key
import Amazonka.S3.Sync.Options.IncludeExclude

data SyncOptions = SyncOptions
  { dryRun :: DryRun
  , includeExcludes :: [IncludeExclude]
  , delete :: Delete
  , sizeOnly :: SizeOnly
  , arguments :: SyncArguments
  }
  deriving stock (Eq, Show)

data DryRun = DryRun | NotDryRun
  deriving stock (Eq, Show)

data Delete = Delete | Don'tDelete
  deriving stock (Eq, Show)

data SizeOnly = SizeOnly | NotSizeOnly
  deriving stock (Eq, Show)

data SyncArguments
  = SyncTo (Path Abs Dir) (BucketKey Abs Prefix)
  | SyncFrom (BucketKey Abs Prefix) (Path Abs Dir)
  | SyncBetween (BucketKey Abs Prefix) (BucketKey Abs Prefix)
  deriving stock (Eq, Show)
