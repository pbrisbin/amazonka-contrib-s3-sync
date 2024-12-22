{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Prelude

import qualified Amazonka
import Amazonka.Data.Text (FromText (..))
import Amazonka.S3.Sync
import Amazonka.S3.Sync.Options
import Path (reldir)

main :: IO ()
main = do
  let
    src = [reldir|src/|]
    dst = either (error . show) id $ fromText "s3://files.pbrisbin.com/docs/"

  env <- Amazonka.newEnv Amazonka.discover

  sync env
    $ SyncOptions
      { dryRun = DryRun
      , includeExcludes = []
      , delete = Delete
      , sizeOnly = NotSizeOnly
      , arguments = SyncTo src dst
      }
