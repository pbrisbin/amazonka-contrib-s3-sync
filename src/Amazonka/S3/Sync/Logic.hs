{-# LANGUAGE TupleSections #-}

module Amazonka.S3.Sync.Logic
  ( SyncLogic (..)
  , runSyncLogic
  )
where

import Amazonka.S3.Sync.Prelude

import Amazonka.S3.Sync.PairUp
import Conduit
import Data.List (sort)

data SyncLogic m sourceDir sourceFile sourceItem targetDir targetFile targetItem = SyncLogic
  { listSource :: sourceDir -> m ([sourceDir], [sourceFile])
  , listTarget :: targetDir -> m ([targetDir], [targetFile])
  , toSourceItem :: sourceDir -> sourceFile -> m sourceItem
  , toTargetItem :: targetDir -> targetFile -> m targetItem
  , compareDir :: sourceDir -> targetDir -> sourceDir -> targetDir -> m Ordering
  , compareItem :: sourceItem -> targetItem -> Ordering
  }

runSyncLogic
  :: ( Monad m
     , Ord sourceDir
     , Ord sourceItem
     , Ord targetDir
     , Ord targetItem
     )
  => SyncLogic m sourceDir sourceFile sourceItem targetDir targetFile targetItem
  -> sourceDir
  -> targetDir
  -> ConduitT i (These sourceItem targetItem) m ()
runSyncLogic logic source target = go $ These source target
 where
  go x = do
    ((sdirs, sfiles), (tdirs, tfiles)) <- lift $ case x of
      This sdir -> (,([], [])) <$> logic.listSource sdir
      That tdir -> (([], []),) <$> logic.listTarget tdir
      These sdir tdir ->
        (,)
          <$> logic.listSource sdir
          <*> logic.listTarget tdir

    pairedDirs <-
      lift
        $ pairUpM
          (logic.compareDir source target)
          (sort sdirs)
          (sort tdirs)

    pairedItems <-
      lift
        $ pairUp logic.compareItem
          <$> (sort <$> traverse (logic.toSourceItem source) sfiles)
          <*> (sort <$> traverse (logic.toTargetItem target) tfiles)

    traverse_ go pairedDirs
    yieldMany pairedItems
