{-# LANGUAGE TupleSections #-}

module Amazonka.S3.Sync
  ( sync
  , syncM
  ) where

import Amazonka.S3.Sync.Prelude

import qualified Amazonka
import Amazonka.S3.Sync.Action
import Amazonka.S3.Sync.ActualIO
import Amazonka.S3.Sync.Folder
import Amazonka.S3.Sync.Item
import Amazonka.S3.Sync.Options
import Amazonka.S3.Sync.PairUp
import Amazonka.S3.Sync.SharedPath
import Amazonka.S3.Sync.Source
import Amazonka.S3.Sync.Target
import Conduit
import Control.Monad.AWS.EnvT
import Control.Monad.Directory
import Control.Monad.Output

sync :: MonadUnliftIO m => Amazonka.Env -> SyncOptions -> m ()
sync env options = void $ runEnvT (runActualIO $ syncM options) env

-- | Fully MTL version of 'sync'
syncM
  :: ( MonadThrow m
     , MonadAWS m
     , MonadDirectory m
     , MonadOutput m
     , ListSource m SyncSourceLocal
     , ListSource m SyncSourceRemote
     , ListTarget m SyncTargetLocal
     , ListTarget m SyncTargetRemote
     )
  => SyncOptions
  -> m [Action]
syncM options = do
  let source = case options.arguments of
        SyncTo a b -> sourceSyncBetween a b $ These a b
        SyncFrom a b -> sourceSyncBetween a b $ These a b
        SyncBetween a b -> sourceSyncBetween a b $ These a b

  runConduit
    $ source
    .| concatMapC (getSyncAction options)
    .| iterMC logOrExecute
    .| sinkList
 where
  logOrExecute
    | options.dryRun == DryRun = logAction "(dryrun) "
    | otherwise = executeAction options

sourceSyncBetween
  :: ( MonadThrow m
     , MonadAWS m
     , MonadDirectory m
     , ToSharedPath SyncItem source
     , ToSharedPath SyncItem target
     , ToSharedPath SyncFolder source
     , ToSharedPath SyncFolder target
     , ListSource m s
     , ListTarget m t
     )
  => source
  -> target
  -> These s t
  -> ConduitT i (These SyncItem SyncItem) m ()
sourceSyncBetween topSource topTarget inputs = do
  ((sFolders, sItems), (tFolders, tItems)) <- lift $ case inputs of
    This s -> (,([], [])) <$> listSource s
    That t -> (([], []),) <$> listTarget t
    These s t -> (,) <$> listSource s <*> listTarget t

  traverse_ (sourceSyncBetween topSource topTarget)
    =<< pairUpM
      ( \sFolder tFolder ->
          compare
            <$> toSharedPath topSource sFolder
            <*> toSharedPath topTarget tFolder
      )
      sFolders
      tFolders

  yieldMany
    =<< pairUpM
      ( \sItem tItem ->
          compare
            <$> toSharedPath topSource sItem
            <*> toSharedPath topTarget tItem
      )
      sItems
      tItems

getSyncAction
  :: SyncOptions
  -> These SyncItem SyncItem
  -> Maybe Action
getSyncAction options = \case
  This source -> do
    guard $ shouldInclude source
    pure $ CreateTarget source
  That target -> do
    guard $ shouldInclude target
    guard $ options.delete == Delete
    pure $ DeleteTarget target
  These source target -> do
    guard $ shouldInclude source
    guard $ shouldInclude target
    guard $ source.size /= target.size
    guard $ options.sizeOnly == SizeOnly || source.mtime > target.mtime
    Just $ UpdateTarget source target
