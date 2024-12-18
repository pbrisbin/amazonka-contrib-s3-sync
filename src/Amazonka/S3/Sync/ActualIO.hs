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
import qualified System.FilePath as FilePath
import qualified UnliftIO.Directory as UnliftIO
import UnliftIO.IORef
import qualified UnliftIO.Path.Directory as UnliftIO.Path

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
  -- UnliftIO.Path.listDirectory is broken, so we re-implement it
  listDirectory d = do
    xs <- UnliftIO.listDirectory dfp
    dirs <- traverse parseRelDir =<< filterM checkDir xs
    fils <- traverse parseRelFile =<< filterM checkFile xs
    pure (dirs, fils)
   where
    dfp = toFilePath d
    checkDir = UnliftIO.doesDirectoryExist . (dfp FilePath.</>)
    checkFile = UnliftIO.doesFileExist . (dfp FilePath.</>)

  doesFileExist = UnliftIO.Path.doesFileExist
  getFileSize = UnliftIO.Path.getFileSize
  getModificationTime = UnliftIO.Path.getModificationTime

instance MonadIO m => MonadPathsRef (ActualIO m) where
  getPathsRef = PathsRef <$> newIORef mempty
  recordSeen (PathsRef ref) f = atomicModifyIORef' ref $ (,()) . Set.insert f
  wasn'tSeen (PathsRef ref) f = not . Set.member f <$> readIORef ref

instance MonadIO m => MonadOutput (ActualIO m) where
  puts = liftIO . T.putStrLn

runActualIO :: ActualIO m a -> m a
runActualIO = (.unwrap)
