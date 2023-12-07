{-# LANGUAGE UndecidableInstances #-}

-- | Basically "Path" but for S3 'ObjectKey's
module Amazonka.S3.Sync.Key
  ( Key
  , Abs
  , Rel
  , Object
  , Prefix
  , KeyException
  , rootKey
  , joinKey
  , stripProperPrefix
  , replaceProperPrefix
  , dropRoot
  , parseAbsPrefix
  , parseRelPrefix
  , parseAbsObject
  , parseRelObject
  , toObjectKey
  , fromAbsPrefix
  , fromRelPrefix
  , fromAbsObject
  , fromRelObject
  , BucketKey (..)
  , rootBucketKey
  , joinBucketKey
  ) where

import Amazonka.S3.Sync.Prelude

import Amazonka.S3.Types (BucketName, ObjectKey (..))
import Control.Monad.Catch (MonadThrow (..))
import qualified Data.Text as T
import UnliftIO.Exception (Exception)

newtype Key b t = Key
  { unwrap :: Text
  }
  deriving stock (Show, Eq)
  deriving newtype (ToText)

instance FromText (Key Abs Prefix) where
  fromText = first show . parseAbsPrefix . ObjectKey

data Object

data Prefix

data KeyException
  = InvalidAbsPrefix ObjectKey
  | InvalidRelPrefix ObjectKey
  | InvalidAbsObject ObjectKey
  | InvalidRelObject ObjectKey
  | InvalidObject ObjectKey
  | InvalidPrefix ObjectKey
  | NotAProperPrefix ObjectKey ObjectKey
  deriving stock (Show)
  deriving anyclass (Exception)

-- | The root, a.k.a an empty Prefix
rootKey :: Key Abs Prefix
rootKey = Key "/"

joinKey :: Key b Prefix -> Key Rel t -> Key b t
joinKey prefix key =
  -- Safe to construct because Prefix ensures a trailing slash, and Rel ensures
  -- no leading slash
  Key $ prefix.unwrap <> key.unwrap

stripProperPrefix :: MonadThrow m => Key b Prefix -> Key b t -> m (Key Rel t)
stripProperPrefix prefix key =
  case T.stripPrefix prefix.unwrap key.unwrap of
    Nothing -> throwM $ NotAProperPrefix (toObjectKey prefix) (toObjectKey key)
    Just t ->
      -- Safe to construct because b ensures leading-slash handling is
      -- consistent, and stripPrefix succeeding with a Prefix (guaranteed to
      -- have trailing-slash) ensures no leading slash will be left after
      -- stripping (TODO: validate against double-slashes in parse?)
      pure $ Key t

replaceProperPrefix
  :: MonadThrow m => Key b Prefix -> Key b' Prefix -> Key b t -> m (Key b' t)
replaceProperPrefix fromPrefix toPrefix key =
  joinKey toPrefix <$> stripProperPrefix fromPrefix key

dropRoot :: Key Abs t -> Maybe (Key Rel t)
dropRoot key = case T.uncons key.unwrap of
  Just ('/', t) | T.null t -> Nothing
  Just ('/', t) -> Just (Key t)
  _ -> error "Key Abs constructed unsafely (no leading slash)"

parseAbsPrefix :: MonadThrow m => ObjectKey -> m (Key Abs Prefix)
parseAbsPrefix key
  | "/" `T.isPrefixOf` t
  , "/" `T.isSuffixOf` t =
      pure $ Key t
  | otherwise = throwM $ InvalidAbsPrefix key
 where
  t = toText key

parseRelPrefix :: MonadThrow m => ObjectKey -> m (Key Rel Prefix)
parseRelPrefix key
  | not ("/" `T.isPrefixOf` t)
  , "/" `T.isSuffixOf` t =
      pure $ Key t
  | otherwise = throwM $ InvalidRelPrefix key
 where
  t = toText key

parseAbsObject :: MonadThrow m => ObjectKey -> m (Key Abs Object)
parseAbsObject key
  | "/" `T.isPrefixOf` t
  , not ("/" `T.isSuffixOf` t) =
      pure $ Key t
  | otherwise = throwM $ InvalidAbsObject key
 where
  t = toText key

parseRelObject :: MonadThrow m => ObjectKey -> m (Key Rel Object)
parseRelObject key
  | not ("/" `T.isPrefixOf` t)
  , not ("/" `T.isSuffixOf` t) =
      pure $ Key t
  | otherwise = throwM $ InvalidRelObject key
 where
  t = toText key

toObjectKey :: Key b t -> ObjectKey
toObjectKey = ObjectKey . (.unwrap)

fromAbsPrefix :: Key Abs Prefix -> ObjectKey
fromAbsPrefix = toObjectKey

fromRelPrefix :: Key Rel Prefix -> ObjectKey
fromRelPrefix = toObjectKey

fromAbsObject :: Key Abs Object -> ObjectKey
fromAbsObject = toObjectKey

fromRelObject :: Key Rel Object -> ObjectKey
fromRelObject = toObjectKey

data BucketKey b t = BucketKey
  { bucket :: BucketName
  , key :: Key b t
  }
  deriving stock (Show)

instance ToText (Key Abs t) => ToText (BucketKey Abs t) where
  toText bk = "s3://" <> toText bk.bucket <> toText bk.key

instance FromText (Key Abs t) => FromText (BucketKey Abs t) where
  fromText t = do
    rest <- note (invalid "must begin with s3://") $ T.stripPrefix "s3://" t
    case T.breakOn "/" rest of
      (b, _) | T.null b -> Left $ invalid "bucket cannot be empty"
      (b, k) | T.null k -> BucketKey <$> fromText b <*> fromText "/"
      (b, k) -> BucketKey <$> fromText b <*> fromText k
   where
    invalid msg = unpack t <> " is not valid as s3://<bucket>[/<key>]: " <> msg

rootBucketKey :: BucketName -> BucketKey Abs Prefix
rootBucketKey bucket =
  BucketKey
    { bucket = bucket
    , key = rootKey
    }

joinBucketKey :: BucketKey b Prefix -> Key Rel t -> BucketKey b t
joinBucketKey bk key =
  BucketKey
    { bucket = bk.bucket
    , key = joinKey bk.key key
    }
