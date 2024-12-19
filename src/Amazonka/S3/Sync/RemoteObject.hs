module Amazonka.S3.Sync.RemoteObject
  ( listRemoteObjects
  ) where

import Amazonka.S3.Sync.Prelude

import Amazonka.S3.ListObjectsV2
  ( listObjectsV2Response_contents
  , listObjectsV2_prefix
  , newListObjectsV2
  )
import Amazonka.S3.Sync.Folder
import Amazonka.S3.Sync.Item
import Amazonka.S3.Sync.Key
import Amazonka.S3.Sync.ObjectAttributes
import Amazonka.S3.Types (object_key)
import qualified Amazonka.S3.Types as S3
import Conduit
import qualified Control.Monad.AWS as AWS
import Data.Either (partitionEithers)

listRemoteObjects
  :: (MonadThrow m, MonadAWS m)
  => BucketKey Abs Prefix
  -> m ([SyncFolder], [SyncItem])
listRemoteObjects bk = do
  s3Objects <-
    runConduit
      $ (AWS.paginateEither req >>= either throwM pure)
      .| concatMapC (^. listObjectsV2Response_contents)
      .| concatC
      .| sinkList

  partitionEithers <$> traverse (toPrefixOrObject bk) s3Objects
 where
  req =
    newListObjectsV2 bk.bucket
      & maybe id (listObjectsV2_prefix ?~) mPrefix

  mPrefix = toText . fromRelPrefix <$> stripProperPrefix rootKey bk.key

toPrefixOrObject
  :: MonadThrow m
  => BucketKey Abs Prefix
  -> S3.Object
  -> m (Either SyncFolder SyncItem)
toPrefixOrObject bk obj =
  let k = obj ^. object_key
  in  case parseRelPrefix k of
        Nothing -> do
          abs <- joinKey rootKey <$> parseRelObject k
          let attrs = getObjectAttributes obj
          pure
            $ Right
            $ SyncItem
              { location = Right $ abs `inBucket` bk.bucket
              , size = attrs.size
              , mtime = attrs.lastModified
              }
        Just pre ->
          pure
            $ Left
            $ SyncFolder
            $ Right
            $ joinKey rootKey pre
            `inBucket` bk.bucket
