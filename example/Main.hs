{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Prelude

import qualified Amazonka
import Amazonka.Data.Text (FromText (..))
import Amazonka.S3.Sync
import Amazonka.S3.Sync.Options
import Path (reldir, (</>))
import Path.IO (getCurrentDir)
import System.Exit (die)

main :: IO ()
main = do
  env <- Amazonka.newEnv Amazonka.discover
  src <- (</> [reldir|./src|]) <$> getCurrentDir
  dst <- either die pure $ fromText "s3://files.pbrisbin.com/docs/"

  syncLocalRemote
    env
    SyncOptions
      { dryRun = DryRun
      , includeExcludes = []
      , delete = Delete
      , sizeOnly = NotSizeOnly
      }
    src
    dst
