{-# LANGUAGE TupleSections #-}

module Amazonka.S3.Sync.ActualIO
  ( ActualIO (..)
  , runActualIO
  ) where

import Amazonka.S3.Sync.Prelude

import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.Directory
import Control.Monad.Output
import Control.Monad.PathsRef
import qualified Data.Set as Set
import qualified Data.Text.IO as T
import qualified Path.IO as Path
import UnliftIO.IORef

newtype ActualIO m a = ActualIO
  { unwrap :: m a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadAWS
    )

instance MonadIO m => MonadThrow (ActualIO m) where
  throwM = liftIO . throwM

instance MonadIO m => MonadDirectory (ActualIO m) where
  listDir = Path.listDir
  doesFileExist = Path.doesFileExist
  getFileSize = Path.getFileSize
  getModificationTime = Path.getModificationTime

instance MonadIO m => MonadPathsRef (ActualIO m) where
  getPathsRef = PathsRef <$> newIORef mempty
  recordSeen (PathsRef ref) f = atomicModifyIORef' ref $ (,()) . Set.insert f
  wasn'tSeen (PathsRef ref) f = not . Set.member f <$> readIORef ref

instance MonadIO m => MonadOutput (ActualIO m) where
  puts = liftIO . T.putStrLn

runActualIO :: ActualIO m a -> m a
runActualIO = (.unwrap)
