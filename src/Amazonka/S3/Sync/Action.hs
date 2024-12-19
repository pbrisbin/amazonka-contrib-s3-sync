module Amazonka.S3.Sync.Action
  ( Action (..)
  , logAction
  , executeAction
  ) where

import Amazonka.S3.Sync.Prelude

import Amazonka.S3.Sync.Options
import Amazonka.S3.Sync.Source
import Control.Monad.Output

data Action
  = DeleteTarget SyncItem
  | UpdateTarget SyncItem SyncItem
  | CreateTarget SyncItem
  deriving stock (Eq, Show)

instance ToText Action where
  toText = \case
    DeleteTarget a -> "delete: " <> toText a
    UpdateTarget a b -> "upload: " <> toText a <> " to " <> toText b
    CreateTarget a -> "upload: " <> toText a <> " to ???"

logAction :: MonadOutput m => Text -> Action -> m ()
logAction prefix = puts . (prefix <>) . toText

executeAction :: MonadOutput m => SyncOptions -> Action -> m ()
executeAction _ = logAction ""
