module Amazonka.S3.Sync.Options
  ( SyncOptions (..)
  , DryRun (..)
  , Delete (..)
  , SizeOnly (..)
  , SyncArguments (..)
  ) where

import Amazonka.S3.Sync.Prelude

import Amazonka.S3.Sync.Options.IncludeExclude
import Amazonka.S3.Sync.Source
import Amazonka.S3.Sync.Target

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
  = SyncTo SyncSourceLocal SyncTargetRemote
  | SyncFrom SyncSourceRemote SyncTargetLocal
  | SyncBetween SyncSourceRemote SyncTargetRemote
  deriving stock (Eq, Show)
