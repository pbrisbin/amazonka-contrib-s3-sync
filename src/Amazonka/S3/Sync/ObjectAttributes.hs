module Amazonka.S3.Sync.ObjectAttributes
  ( ObjectAttributes (..)
  , getObjectAttributes
  , listPrefixWithObjectAttributes
  )
where

import Amazonka.S3.Sync.Prelude

import Amazonka.S3.ListObjectsV2
  ( ListObjectsV2Response
  , listObjectsV2Response_commonPrefixes
  , listObjectsV2Response_contents
  , listObjectsV2_delimiter
  , listObjectsV2_prefix
  , newListObjectsV2
  )
import Amazonka.S3.Sync.Key
import qualified Amazonka.S3.Types as S3
import Amazonka.S3.Types.Object (object_key, object_lastModified, object_size)
import Conduit
import qualified Control.Monad.AWS as AWS

data ObjectAttributes = ObjectAttributes
  { size :: Integer
  , lastModified :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

getObjectAttributes :: S3.Object -> ObjectAttributes
getObjectAttributes obj =
  ObjectAttributes
    { size = obj ^. object_size
    , lastModified = obj ^. object_lastModified
    }

listPrefixWithObjectAttributes
  :: (MonadThrow m, MonadAWS m)
  => BucketKey Abs Prefix
  -> m ([BucketKey Abs Prefix], [(Key Abs Object, ObjectAttributes)])
listPrefixWithObjectAttributes bk = do
  runConduit
    $ (AWS.paginateEither req >>= either throwM pure)
      .| responseToEither
      .| eitherToResult
      .| foldC
 where
  req =
    newListObjectsV2 bk.bucket
      & maybe id (listObjectsV2_prefix ?~) mPrefix
      & (listObjectsV2_delimiter ?~ '/')

  mPrefix = toText . fromRelPrefix <$> stripProperPrefix rootKey bk.key

responseToEither
  :: Monad m
  => ConduitT ListObjectsV2Response (Either S3.CommonPrefix S3.Object) m ()
responseToEither = awaitForever $ \r -> do
  traverse_ (yieldMany . map Left) $ r ^. listObjectsV2Response_commonPrefixes
  traverse_ (yieldMany . map Right) $ r ^. listObjectsV2Response_contents

eitherToResult
  :: MonadThrow m
  => ConduitT
      (Either S3.CommonPrefix S3.Object)
      ([BucketKey Abs Prefix], [(Key Abs Object, ObjectAttributes)])
      m
      ()
eitherToResult =
  awaitForever
    $ either
      (yield . (\p -> ([p], [])) <=< commonPrefixToPrefix)
      (yield . (\i -> ([], [i])) <=< objectToItem)

commonPrefixToPrefix :: S3.CommonPrefix -> m (BucketKey Abs Prefix)
commonPrefixToPrefix = undefined

objectToItem
  :: MonadThrow m => S3.Object -> m (Key Abs Object, ObjectAttributes)
objectToItem obj = do
  rel <- parseRelObject $ obj ^. object_key
  pure (joinKey rootKey rel, getObjectAttributes obj)
