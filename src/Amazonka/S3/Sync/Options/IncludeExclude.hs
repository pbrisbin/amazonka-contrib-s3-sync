module Amazonka.S3.Sync.Options.IncludeExclude
  ( IncludeExclude (..)
  , shouldIncludePath
  , shouldIncludeText
  , shouldInclude
  ) where

import Amazonka.S3.Sync.Prelude

import Data.Monoid (Last (..))
import System.FilePath.Glob (match)

data IncludeExclude
  = Include Pattern
  | Exclude Pattern
  deriving stock (Eq, Show)

shouldIncludePath :: Path b t -> [IncludeExclude] -> Bool
shouldIncludePath = shouldInclude . toFilePath

shouldIncludeText :: Text -> [IncludeExclude] -> Bool
shouldIncludeText = shouldInclude . unpack

shouldInclude :: String -> [IncludeExclude] -> Bool
shouldInclude fpath =
  fromMaybe True . getLast . foldMap decide
 where
  decide =
    Last . \case
      Include ptn | ptn `match` fpath -> Just True
      Exclude ptn | ptn `match` fpath -> Just False
      _ -> Nothing
