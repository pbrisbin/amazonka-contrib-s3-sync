module Amazonka.S3.Sync.Action
  ( Action (..)
  , shouldExecuteAction
  , executeAction
  , logAction
  ) where

import Amazonka.S3.Sync.Prelude

import Amazonka.S3.Sync.FileDetails
import Amazonka.S3.Sync.ObjectAttributes
import Amazonka.S3.Sync.Options
import Amazonka.S3.Sync.PairedItem
import Control.Monad.Output

data Action
  = DeleteObject (PairedItem NoDetails)
  | UpdateObject (PairedItem FileObject)
  | CreateObject (PairedItem NoDetails)
  deriving stock (Eq, Show)

shouldExecuteAction :: SyncOptions -> Action -> Bool
shouldExecuteAction options = \case
  DeleteObject {} | options.delete == Delete -> True
  UpdateObject p
    | p.details.fileDetails.size /= p.details.objectAttributes.size
    , options.sizeOnly == SizeOnly ->
        True
    | p.details.fileDetails.size /= p.details.objectAttributes.size
    , p.details.fileDetails.mtime > p.details.objectAttributes.lastModified ->
        True
  CreateObject {} -> True
  _ -> False

executeAction :: MonadOutput m => SyncOptions -> Action -> m ()
executeAction _ = logAction ""

logAction :: MonadOutput m => Text -> Action -> m ()
logAction prefix =
  puts . (prefix <>) . \case
    DeleteObject p ->
      "delete: " <> toText p.object
    UpdateObject p ->
      "upload: " <> pack (toFilePath p.file) <> " to " <> toText p.object
    CreateObject p ->
      "upload: " <> pack (toFilePath p.file) <> " to " <> toText p.object
