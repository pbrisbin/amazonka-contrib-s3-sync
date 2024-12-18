module Amazonka.S3.Sync.PairedItem
  ( PairedItem (..)
  , NoDetails (..)
  , FileOnly (..)
  , ObjectOnly (..)
  , FileObject (..)
  , streamDirectoryPairedItems
  , streamBucketKeyPairedItems
  ) where

import Amazonka.S3.Sync.Prelude

import Amazonka.S3.ListObjectsV2
  ( listObjectsV2Response_contents
  , listObjectsV2_prefix
  , newListObjectsV2
  )
import Amazonka.S3.Sync.FileDetails
import Amazonka.S3.Sync.Key
import Amazonka.S3.Sync.ObjectAttributes
import Amazonka.S3.Types (ObjectKey (..), object_key)
import qualified Amazonka.S3.Types as S3
import Conduit
import qualified Control.Monad.AWS as AWS
import Control.Monad.Directory
import qualified Path
import qualified System.FilePath as FilePath

data PairedItem details = PairedItem
  { file :: Path Abs File
  , object :: BucketKey Abs Object
  , details :: details
  }
  deriving stock (Eq, Show, Functor)

data NoDetails = NoDetails
  deriving stock (Eq, Show)

newtype FileOnly = FileOnly
  { unwrap :: FileDetails
  }
  deriving stock (Eq, Show)

newtype ObjectOnly = ObjectOnly
  { unwrap :: ObjectAttributes
  }
  deriving stock (Eq, Show)

data FileObject = FileObject
  { fileDetails :: FileDetails
  , objectAttributes :: ObjectAttributes
  }
  deriving stock (Eq, Show)

sourceRecursiveDescent
  :: Monad m
  => (folder -> m ([folder], [node]))
  -- ^ List the contents of a folder as more folders or nodes
  --
  -- For local, folder is 'Dir' and node is 'File'. For remote, folder is
  -- 'Prefix' and node is 'Object'
  -> (folder -> node -> Maybe a)
  -- ^ Action to take on each node, given the starting folder as an argument
  -> folder
  -- ^ Starting folder
  -> ConduitT i a m ()
sourceRecursiveDescent list process top = loop top
 where
  loop d = do
    (dirs, items) <- lift $ list d
    yieldMany items .| concatMapC (process top)
    traverse_ loop dirs

streamDirectoryPairedItems
  :: MonadDirectory m
  => BucketKey Abs Prefix
  -> Path Abs Dir
  -> ConduitT i (PairedItem NoDetails) m ()
streamDirectoryPairedItems bk = sourceRecursiveDescent listDir $ \d file -> do
  relKey <-
    parseRelObject
      . ObjectKey
      . pack
      . toPosixPath
      . toFilePath
      =<< Path.stripProperPrefix d file

  pure
    $ PairedItem
      { file = file
      , object = joinBucketKey bk relKey
      , details = NoDetails
      }

streamBucketKeyPairedItems
  :: (MonadThrow m, MonadAWS m)
  => Path Abs Dir
  -> BucketKey Abs Prefix
  -> ConduitT () (PairedItem ObjectOnly) m ()
streamBucketKeyPairedItems dir = sourceRecursiveDescent listBucketKeyAbs $ \bk obj -> do
  absKey <- joinKey rootKey <$> parseRelObject (obj ^. object_key)
  relFile <-
    parseRelFile
      . fromPosixPath
      . unpack
      . toText
      . toObjectKey
      =<< stripProperPrefix bk.key absKey

  pure
    $ PairedItem
      { file = dir </> relFile
      , object = bk {key = absKey}
      , details = ObjectOnly $ getObjectAttributes obj
      }

listBucketKeyAbs
  :: (MonadThrow m, MonadAWS m)
  => BucketKey Abs Prefix
  -> m ([BucketKey Abs Prefix], [S3.Object])
listBucketKeyAbs bk = do
  runConduit
    $ (AWS.paginateEither req >>= either throwM pure)
    .| concatMapC (^. listObjectsV2Response_contents)
    .| concatC
    .| foldMapC (\obj -> maybe ([], [obj]) (\p -> ([p], [])) $ toPrefix obj)
 where
  mPrefix = toText . fromRelPrefix <$> stripProperPrefix rootKey bk.key

  req =
    newListObjectsV2 bk.bucket
      & maybe id (listObjectsV2_prefix ?~) mPrefix

  toPrefix :: S3.Object -> Maybe (BucketKey Abs Prefix)
  toPrefix =
    fmap (joinBucketKey $ rootBucketKey bk.bucket)
      . parseRelPrefix
      . (^. object_key)

toPosixPath :: FilePath -> String
toPosixPath = map $ \case
  c | FilePath.isPathSeparator c -> '/'
  c -> c

fromPosixPath :: String -> FilePath
fromPosixPath = map $ \case
  '/' -> FilePath.pathSeparator
  c -> c
