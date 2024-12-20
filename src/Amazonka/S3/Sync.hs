module Amazonka.S3.Sync
  ( syncLocalRemote
  , syncLocalRemoteM
  ) where

import Amazonka.S3.Sync.Prelude

import qualified Amazonka
import Amazonka.S3.Sync.Action
import Amazonka.S3.Sync.ActualIO
import Amazonka.S3.Sync.FileDetails
import Amazonka.S3.Sync.Key
import Amazonka.S3.Sync.Options
import Amazonka.S3.Sync.Options.IncludeExclude
import Amazonka.S3.Sync.PairedItem
import Conduit
import Control.Monad.AWS.EnvT
import Control.Monad.Directory
import Control.Monad.Output
import Control.Monad.PathsRef
import qualified Path

syncLocalRemote
  :: MonadUnliftIO m
  => Amazonka.Env
  -> SyncOptions
  -> Path Abs Dir
  -> BucketKey Abs Prefix
  -> m ()
syncLocalRemote env options src dst =
  void $ runEnvT (runActualIO $ syncLocalRemoteM options src dst) env

-- | Fully MTL version of 'syncLocalRemote', for testing
syncLocalRemoteM
  :: ( MonadThrow m
     , MonadAWS m
     , MonadDirectory m
     , MonadPathsRef m
     , MonadOutput m
     )
  => SyncOptions
  -> Path Abs Dir
  -> BucketKey Abs Prefix
  -> m [Action]
syncLocalRemoteM options src dst = do
  ref <- getPathsRef

  runConduit
    $ do
      streamBucketKeyPairedItems src dst
        .| filterMC include
        .| iterMC (recordSeen ref . (.file))
        .| toUpdateDelete
      streamDirectoryPairedItems dst src
        .| filterMC include
        .| filterMC (wasn'tSeen ref . (.file))
        .| mapC CreateObject
    .| filterC (shouldExecuteAction options)
    .| iterMC logOrExecute
    .| sinkList
 where
  include :: MonadThrow m => PairedItem details -> m Bool
  include p = do
    rel <- Path.stripProperPrefix src p.file
    pure $ shouldIncludePath rel options.includeExcludes

  toUpdateDelete = awaitForever $ \p -> do
    mDetails <- lift $ getFileDetails p.file

    yield $ case mDetails of
      Nothing -> DeleteObject $ NoDetails <$ p
      Just fd -> UpdateObject $ FileObject fd . (.unwrap) <$> p

  logOrExecute
    | options.dryRun == DryRun = logAction "(dryrun) "
    | otherwise = executeAction options
