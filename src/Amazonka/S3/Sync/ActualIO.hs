module Amazonka.S3.Sync.ActualIO
  ( ActualIO (..)
  , runActualIO
  ) where

import Amazonka.S3.Sync.Prelude

import Control.Monad.Directory
import Control.Monad.Output
import qualified Data.Text.IO as T
import qualified Path.IO as Path

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
  listDirRel d = do
    contents <- Path.listDirRel d
    pure $ bimap (map (d </>)) (map (d </>)) contents
  doesFileExist = Path.doesFileExist
  getFileSize = Path.getFileSize
  getModificationTime = Path.getModificationTime

instance MonadIO m => MonadOutput (ActualIO m) where
  puts = liftIO . T.putStrLn

runActualIO :: ActualIO m a -> m a
runActualIO = (.unwrap)
