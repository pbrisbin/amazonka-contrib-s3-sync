module Amazonka.S3.Sync
  ( sync
  , syncM
  ) where

import Amazonka.S3.Sync.Prelude

import qualified Amazonka
import Amazonka.S3.Sync.Action
import Amazonka.S3.Sync.ActualIO
import Amazonka.S3.Sync.Item
import Amazonka.S3.Sync.Options
import Amazonka.S3.Sync.Options.IncludeExclude
import Amazonka.S3.Sync.Source
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
     )
  => SyncOptions
  -> m [Action]
syncM options =
  runConduit $ source .| iterMC (executeAction options) .| sinkList
 where
  source = case options.arguments of
    SyncTo a b ->
      sourceLocalRemote a b
        .| filterC (includeSyncItem options)
        .| mapC (actionLocalRemote a b)
    SyncFrom a b ->
      sourceRemoteLocal a b
        .| filterC (includeSyncItem options)
        .| mapC (actionRemoteLocal a b)
    SyncBetween a b ->
      sourceRemoteRemote a b
        .| filterC (includeSyncItem options)
        .| mapC (actionRemoteRemote a b)

includeSyncItem
  :: (ToText (ks Rel ts), ToText (kt Rel tt))
  => SyncOptions
  -> These (SyncItem ks ts) (SyncItem kt tt)
  -> Bool
includeSyncItem options = \case
  This source ->
    shouldIncludeText (toText source) options.includeExcludes
  That target ->
    and
      [ shouldIncludeText (toText target) options.includeExcludes
      , options.delete == Delete
      ]
  These source target ->
    and
      [ shouldIncludeText (toText source) options.includeExcludes
      , shouldIncludeText (toText target) options.includeExcludes
      , source.size /= target.size
      , options.sizeOnly == SizeOnly || source.mtime > target.mtime
      ]
