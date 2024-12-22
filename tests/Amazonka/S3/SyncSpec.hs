{-# LANGUAGE QuasiQuotes #-}

module Amazonka.S3.SyncSpec
  ( spec
  ) where

import Amazonka.S3.Sync.Prelude

import Amazonka.S3.Sync
import Amazonka.S3.Sync.Key
import Amazonka.S3.Sync.Mocks
import Amazonka.S3.Sync.Options
import Amazonka.S3.Sync.Options.IncludeExclude
import Path (reldir)
import Test.Hspec

src :: Path Rel Dir
src = [reldir|src/|]

dst :: HasCallStack => BucketKey Abs Prefix
dst = either error id $ fromText "s3://bucket/dst/"

spec :: Spec
spec = do
  describe "syncM" $ do
    context "not nested" $ do
      mocks <-
        runIO
          $ Mocks
            <$> mockDir
              [ "src/foo.txt 123 2024-01-01T12:00:00.0Z"
              , "src/fox.txt 123 2024-01-01T12:00:00.0Z"
              , "src/bar.txt 457 2024-01-02T10:00:00.0Z"
              ]
            <*> mockAWS
              [ "s3://bucket/dst/foo.txt 124 2024-01-01T11:59:00.0Z"
              , "s3://bucket/dst/fox.txt 124 2024-01-01T12:00:00.0Z"
              , "s3://bucket/dst/baz.txt 140 2024-01-03T11:00:00.0Z"
              ]

      let
        baseOptions :: SyncOptions
        baseOptions =
          SyncOptions
            { dryRun = DryRun
            , includeExcludes = []
            , delete = Don'tDelete
            , sizeOnly = NotSizeOnly
            , arguments = SyncTo src dst
            }

      it "works without delete or size-only" $ do
        let options = baseOptions

        fmap (map toText) (runMocksM (syncM options) mocks)
          `shouldReturn` [ "upload: src/bar.txt to s3://bucket/dst/bar.txt"
                         , "upload: src/foo.txt to s3://bucket/dst/foo.txt"
                         ]

      it "works with size-only" $ do
        let options = baseOptions {sizeOnly = SizeOnly}

        fmap (map toText) (runMocksM (syncM options) mocks)
          `shouldReturn` [ "upload: src/bar.txt to s3://bucket/dst/bar.txt"
                         , "upload: src/foo.txt to s3://bucket/dst/foo.txt"
                         , "upload: src/fox.txt to s3://bucket/dst/fox.txt"
                         ]

      it "works with delete" $ do
        let options = baseOptions {delete = Delete}

        fmap (map toText) (runMocksM (syncM options) mocks)
          `shouldReturn` [ "upload: src/bar.txt to s3://bucket/dst/bar.txt"
                         , "delete: s3://bucket/dst/baz.txt"
                         , "upload: src/foo.txt to s3://bucket/dst/foo.txt"
                         ]

      it "works with include-excludes" $ do
        pendingWith "include-exclude"

        let options = baseOptions {includeExcludes = [Exclude "fo*.txt", Include "fox.txt"]}

        fmap (map toText) (runMocksM (syncM options) mocks)
          `shouldReturn` [ "upload: src/bar.txt to s3://bucket/dst/bar.txt"
                         , "upload: src/fox.txt to s3://bucket/dst/fox.txt"
                         ]
