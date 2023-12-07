{-# LANGUAGE ViewPatterns #-}

module Amazonka.S3.Sync.Options.IncludeExclude
  ( IncludeExclude (..)
  , shouldIncludePath
  ) where

import Amazonka.S3.Sync.Prelude

import Data.Monoid (Last (..))
import System.FilePath.Glob (match)

data IncludeExclude
  = Include Pattern
  | Exclude Pattern

shouldIncludePath :: Path b t -> [IncludeExclude] -> Bool
shouldIncludePath (toFilePath -> fpath) =
  fromMaybe True . getLast . foldMap decide
 where
  decide =
    Last . \case
      Include ptn | ptn `match` fpath -> Just True
      Exclude ptn | ptn `match` fpath -> Just False
      _ -> Nothing
