module Amazonka.S3.Sync.Options
  ( SyncOptions (..)
  , DryRun (..)
  , Delete (..)
  , SizeOnly (..)
  , addSyncOptionsInclude
  , addSyncOptionsExclude
  ) where

import Amazonka.S3.Sync.Prelude

import Amazonka.S3.Sync.Options.IncludeExclude

data SyncOptions = SyncOptions
  { dryRun :: DryRun
  , includeExcludes :: [IncludeExclude]
  , delete :: Delete
  , sizeOnly :: SizeOnly
  }

data DryRun = DryRun | NotDryRun
  deriving stock (Eq)

data Delete = Delete | Don'tDelete
  deriving stock (Eq)

data SizeOnly = SizeOnly | NotSizeOnly
  deriving stock (Eq)

addSyncOptionsInclude :: Pattern -> SyncOptions -> SyncOptions
addSyncOptionsInclude p so = so {includeExcludes = so.includeExcludes <> [Include p]}

addSyncOptionsExclude :: Pattern -> SyncOptions -> SyncOptions
addSyncOptionsExclude p so = so {includeExcludes = so.includeExcludes <> [Exclude p]}
