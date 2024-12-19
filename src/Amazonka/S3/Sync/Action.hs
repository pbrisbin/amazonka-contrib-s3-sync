module Amazonka.S3.Sync.Action
  ( Action (..)
  , actionLocalRemote
  , actionRemoteLocal
  , actionRemoteRemote
  , executeAction
  ) where

import Amazonka.S3.Sync.Prelude

import Amazonka.S3.Sync.Item
import Amazonka.S3.Sync.Key
import Amazonka.S3.Sync.Options
import Amazonka.S3.Types (ObjectKey (..))
import Control.Monad.Output

data Action
  = DeleteFile (Path Abs File)
  | DeleteObject (BucketKey Abs Object)
  | CopyObjectToFile (BucketKey Abs Object) (Path Abs File)
  | CopyFileToObject (Path Abs File) (BucketKey Abs Object)
  deriving stock (Eq, Show)

instance ToText Action where
  toText = \case
    DeleteFile a -> "delete: " <> toText a
    DeleteObject a -> "delete: " <> toText a
    CopyObjectToFile a b -> "upload: " <> toText a <> " to " <> toText b
    CopyFileToObject a b -> "upload: " <> toText a <> " to " <> toText b

actionLocalRemote
  :: Path Abs Dir
  -> BucketKey Abs Prefix
  -> These (SyncItem Path File) (SyncItem Key Object)
  -> Action
actionLocalRemote dir prefix = \case
  This item ->
    let
      file = item.location
      rel = fileToObject file
      abs = joinKey prefix.key rel
    in
      CopyFileToObject (dir </> file) $ abs `inBucket` prefix.bucket
  That item ->
    let
      rel = item.location
      abs = joinKey prefix.key rel
    in
      DeleteObject $ abs `inBucket` prefix.bucket
  These sItem tItem ->
    let
      file = sItem.location
      rel = tItem.location
      abs = joinKey prefix.key rel
    in
      CopyFileToObject (dir </> file) $ abs `inBucket` prefix.bucket

actionRemoteLocal
  :: BucketKey Abs Prefix
  -> Path Abs Dir
  -> These (SyncItem Key Object) (SyncItem Path File)
  -> Action
actionRemoteLocal = undefined

actionRemoteRemote
  :: BucketKey Abs Prefix
  -> BucketKey Abs Prefix
  -> These (SyncItem Key Object) (SyncItem Key Object)
  -> Action
actionRemoteRemote = undefined

fileToObject :: Path Rel File -> Key Rel Object
fileToObject file =
  either (error . show) id
    $ parseRelObject
    $ ObjectKey
    $ pack
    $ toFilePath file

executeAction :: MonadOutput m => SyncOptions -> Action -> m ()
executeAction options
  | options.dryRun == DryRun = logAction "(dryrun) "
  | otherwise = logAction "" -- TODO

logAction :: MonadOutput m => Text -> Action -> m ()
logAction prefix = puts . (prefix <>) . toText
