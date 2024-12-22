module Amazonka.S3.Sync.Action
  ( Action (..)
  , actionLocalRemote
  , actionRemoteLocal
  , actionRemoteRemote
  , executeAction
  ) where

import Amazonka.S3.Sync.Prelude

import Amazonka.S3.Sync.CompareKey
import Amazonka.S3.Sync.Item
import Amazonka.S3.Sync.Key
import Amazonka.S3.Sync.Options
import Amazonka.S3.Types (ObjectKey (..))
import Control.Monad.Output

data Action
  = DeleteFile (Path Rel File)
  | DeleteObject (BucketKey Abs Object)
  | Download (BucketKey Abs Object) (Path Rel File)
  | Upload (Path Rel File) (BucketKey Abs Object)
  | Copy (BucketKey Abs Object) (BucketKey Abs Object)
  deriving stock (Eq, Show)

instance ToText Action where
  toText = \case
    DeleteFile a -> "delete: " <> toText a
    DeleteObject a -> "delete: " <> toText a
    Download a b -> "download: " <> toText a <> " to " <> toText b
    Upload a b -> "upload: " <> toText a <> " to " <> toText b
    Copy a b -> "copy: " <> toText a <> " to " <> toText b

actionLocalRemote
  :: Path Rel Dir
  -> BucketKey Abs Prefix
  -> These (SyncItem Path File) (SyncItem Key Object)
  -> Action
actionLocalRemote =
  actionLogic
    (</>)
    (\bk k -> joinKey bk.key k `inBucket` bk.bucket)
    fileToObject
    DeleteObject
    Upload

actionRemoteLocal
  :: BucketKey Abs Prefix
  -> Path Rel Dir
  -> These (SyncItem Key Object) (SyncItem Path File)
  -> Action
actionRemoteLocal =
  actionLogic
    (\bk k -> joinKey bk.key k `inBucket` bk.bucket)
    (</>)
    objectToFile
    DeleteFile
    Download

actionRemoteRemote
  :: BucketKey Abs Prefix
  -> BucketKey Abs Prefix
  -> These (SyncItem Key Object) (SyncItem Key Object)
  -> Action
actionRemoteRemote =
  actionLogic
    (\bk k -> joinKey bk.key k `inBucket` bk.bucket)
    (\bk k -> joinKey bk.key k `inBucket` bk.bucket)
    id
    DeleteObject
    Copy

actionLogic
  :: (sBucketKey sAbs sPrefix -> sKey Rel sObject -> sBucketKey sAbs sObject)
  -> (tBucketKey tAbs tPrefix -> tKey Rel tObject -> tBucketKey tAbs tObject)
  -> (sKey Rel sObject -> tKey Rel tObject)
  -> (tBucketKey tAbs tObject -> Action)
  -> (sBucketKey sAbs sObject -> tBucketKey tAbs tObject -> Action)
  -> sBucketKey sAbs sPrefix
  -> tBucketKey tAbs tPrefix
  -> These (SyncItem sKey sObject) (SyncItem tKey tObject)
  -> Action
actionLogic inSourceFn inTargetFn transFn deleteFn copyFn source target = \case
  This item ->
    copyFn (inSourceFn source item.location)
      $ inTargetFn target
      $ transFn item.location
  That item -> deleteFn $ inTargetFn target item.location
  These sItem tItem -> copyFn (inSourceFn source sItem.location) $ inTargetFn target tItem.location

fileToObject :: Path Rel File -> Key Rel Object
fileToObject = viaCompareKey $ parseRelObject . ObjectKey

objectToFile :: Key Rel Object -> Path Rel File
objectToFile = viaCompareKey $ parseRelFile . unpack

viaCompareKey
  :: (Show e, ToCompareKey (k1 Rel t1))
  => (Text -> Either e (k2 Rel t2))
  -> k1 Rel t1
  -> k2 Rel t2
viaCompareKey f = either (error . show) id . f . toCompareKey

executeAction :: MonadOutput m => SyncOptions -> Action -> m ()
executeAction options
  | options.dryRun == DryRun = puts . ("(dryrun) " <>) . toText
  | otherwise = \action -> do
      puts $ toText action
      error "S3 operations not yet implemented"
