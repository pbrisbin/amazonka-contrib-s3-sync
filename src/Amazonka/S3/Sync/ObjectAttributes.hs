module Amazonka.S3.Sync.ObjectAttributes
  ( ObjectAttributes (..)
  , listPrefixWithObjectAttributes
  )
where

import Amazonka.S3.Sync.Prelude

import Amazonka.S3.ListObjectsV2
  ( ListObjectsV2Response
  , listObjectsV2Response_commonPrefixes
  , listObjectsV2Response_contents
  , listObjectsV2Response_prefix
  , listObjectsV2_delimiter
  , listObjectsV2_prefix
  , newListObjectsV2
  )
import Amazonka.S3.Sync.Key
import qualified Amazonka.S3.Types as S3
import Amazonka.S3.Types.CommonPrefix (commonPrefix_prefix)
import Amazonka.S3.Types.Object (object_key, object_lastModified, object_size)
import Conduit
import qualified Control.Monad.AWS as AWS

data ObjectAttributes = ObjectAttributes
  { size :: Integer
  , lastModified :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

listPrefixWithObjectAttributes
  :: (MonadThrow m, MonadAWS m)
  => BucketKey Abs Prefix
  -> m ([BucketKey Abs Prefix], [(Key Abs Object, ObjectAttributes)])
listPrefixWithObjectAttributes bk = do
  runConduit
    $ (AWS.paginateEither req >>= either throwM pure)
      .| responseToEither bk
      .| eitherToResult
      .| foldC
 where
  req =
    newListObjectsV2 bk.bucket
      & maybe id (listObjectsV2_prefix ?~) mPrefix
      & (listObjectsV2_delimiter ?~ '/')

  mPrefix = toText . fromRelPrefix <$> stripProperPrefix rootKey bk.key

responseToEither
  :: MonadThrow m
  => BucketKey Abs Prefix
  -> ConduitT
      ListObjectsV2Response
      (Either (BucketKey Abs Prefix) (Key Abs Object, ObjectAttributes))
      m
      ()
responseToEither bk = awaitForever $ \r -> do
  for_ (r ^. listObjectsV2Response_commonPrefixes) $ \commonPrefixes -> do
    for_ commonPrefixes $ \commonPrefix -> do
      for_ (commonPrefix ^. commonPrefix_prefix) $ \t -> do
        rel <- parseRelPrefix $ S3.ObjectKey t
        yield $ Left $ joinKey rootKey rel `inBucket` bk.bucket

  for_ (r ^. listObjectsV2Response_contents) $ \objs -> do
    for_ objs $ \obj -> do
      let
        k = obj ^. object_key

        -- S3 returns the prefix itself as a key, which we should ignore. You
        -- can confirm this by comparing `aws s3 ls s3://b/p/` with `aws s3api
        -- list-objects-v2 --bucket b --delimiter / --prefix p`. The latter
        -- includes this key, but the former does not.
        isPrefixKey = r ^. listObjectsV2Response_prefix == Just (toText k)

      unless isPrefixKey $ do
        rel <- parseRelObject $ obj ^. object_key
        yield $ Right (joinKey rootKey rel, getObjectAttributes obj)

getObjectAttributes :: S3.Object -> ObjectAttributes
getObjectAttributes obj =
  ObjectAttributes
    { size = obj ^. object_size
    , lastModified = obj ^. object_lastModified
    }

eitherToResult :: Monad m => ConduitT (Either a b) ([a], [b]) m ()
eitherToResult =
  awaitForever $ either (yield . toFstSingleton) (yield . toSndSingleton)
 where
  toFstSingleton x = ([x], [])
  toSndSingleton y = ([], [y])
