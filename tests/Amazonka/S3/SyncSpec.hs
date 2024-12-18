{-# LANGUAGE QuasiQuotes #-}

module Amazonka.S3.SyncSpec
  ( spec
  ) where

import Amazonka.S3.Sync.Prelude

import Amazonka.S3.Sync
import Amazonka.S3.Sync.Action
import Amazonka.S3.Sync.FileDetails
import Amazonka.S3.Sync.Key (BucketKey, Prefix)
import qualified Amazonka.S3.Sync.Key as Key
import Amazonka.S3.Sync.Mocks
import Amazonka.S3.Sync.ObjectAttributes
import Amazonka.S3.Sync.Options
import Amazonka.S3.Sync.Options.IncludeExclude
import Amazonka.S3.Sync.PairedItem
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Path (absdir, absfile)
import Test.Hspec

src :: Path Abs Dir
src = [absdir|/src/|]

dst :: HasCallStack => BucketKey Key.Abs Prefix
dst = Key.joinBucketKey (Key.rootBucketKey "bucket") $ fromTextUnsafe "dst/"

spec :: Spec
spec = do
  describe "syncLocalRemoteM" $ do
    context "not nested" $ do
      mocks <-
        runIO
          $ Mocks
          <$> mockDir
            [ "/src/foo.txt 123 2024-01-01T12:00:00.0Z"
            , "/src/fox.txt 123 2024-01-01T12:00:00.0Z"
            , "/src/bar.txt 457 2024-01-02T10:00:00.0Z"
            ]
          <*> mockAWS
            [ "s3://bucket/dst/foo.txt 124 2024-01-01T11:59:00.0Z"
            , "s3://bucket/dst/fox.txt 124 2024-01-01T12:00:00.0Z"
            , "s3://bucket/dst/baz.txt 140 2024-01-03T11:00:00.0Z"
            ]

      let
        updateFoo :: Action
        updateFoo =
          UpdateObject
            $ PairedItem
              { file = [absfile|/src/foo.txt|]
              , object = fromTextUnsafe "s3://bucket/dst/foo.txt"
              , details =
                  FileObject
                    { fileDetails =
                        FileDetails
                          { size = 123
                          , mtime = makeUTCTime 2024 1 1 12 0
                          }
                    , objectAttributes =
                        ObjectAttributes
                          { size = 124
                          , lastModified = makeUTCTime 2024 1 1 11 59
                          }
                    }
              }

        updateFox :: Action
        updateFox =
          UpdateObject
            $ PairedItem
              { file = [absfile|/src/fox.txt|]
              , object = fromTextUnsafe "s3://bucket/dst/fox.txt"
              , details =
                  FileObject
                    { fileDetails =
                        FileDetails
                          { size = 123
                          , mtime = makeUTCTime 2024 1 1 12 0
                          }
                    , objectAttributes =
                        ObjectAttributes
                          { size = 124
                          , lastModified = makeUTCTime 2024 1 1 12 0
                          }
                    }
              }

        createBar :: Action
        createBar =
          CreateObject
            $ PairedItem
              { file = [absfile|/src/bar.txt|]
              , object = fromTextUnsafe "s3://bucket/dst/bar.txt"
              , details = NoDetails
              }

        deleteBaz :: Action
        deleteBaz =
          DeleteObject
            $ PairedItem
              { file = [absfile|/src/baz.txt|]
              , object = fromTextUnsafe "s3://bucket/dst/baz.txt"
              , details = NoDetails
              }

      it "works without delete or size-only" $ do
        let options =
              SyncOptions
                { dryRun = DryRun
                , includeExcludes = []
                , delete = Don'tDelete
                , sizeOnly = NotSizeOnly
                }

        runMocksM (syncLocalRemoteM options src dst) mocks
          `shouldReturn` [updateFoo, createBar]

      it "works with size-only" $ do
        let options =
              SyncOptions
                { dryRun = DryRun
                , includeExcludes = []
                , delete = Don'tDelete
                , sizeOnly = SizeOnly
                }

        runMocksM (syncLocalRemoteM options src dst) mocks
          `shouldReturn` [updateFoo, updateFox, createBar]

      it "works with delete" $ do
        let options =
              SyncOptions
                { dryRun = DryRun
                , includeExcludes = []
                , delete = Delete
                , sizeOnly = NotSizeOnly
                }

        runMocksM (syncLocalRemoteM options src dst) mocks
          `shouldReturn` [updateFoo, deleteBaz, createBar]

      it "works with include-excludes" $ do
        let options =
              SyncOptions
                { dryRun = DryRun
                , includeExcludes = [Exclude "fo*.txt", Include "fox.txt"]
                , delete = Delete
                , sizeOnly = NotSizeOnly
                }

        runMocksM (syncLocalRemoteM options src dst) mocks
          `shouldReturn` [deleteBaz, createBar]

makeUTCTime :: Integer -> Int -> Int -> Integer -> Integer -> UTCTime
makeUTCTime year month day hour minute =
  UTCTime
    { utctDay = fromGregorian year month day
    , utctDayTime = secondsToDiffTime seconds
    }
 where
  seconds :: Integer
  seconds = ((hour * 60) + minute) * 60

fromTextUnsafe :: (FromText a, HasCallStack) => Text -> a
fromTextUnsafe = either error id . fromText
