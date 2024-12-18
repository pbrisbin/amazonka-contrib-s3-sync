{-# LANGUAGE QuasiQuotes #-}

module Amazonka.S3.SyncSpec
  ( spec
  ) where

import Amazonka.S3.Sync.Prelude

import Amazonka.S3.Sync
import Amazonka.S3.Sync.Action
import Amazonka.S3.Sync.FileDetails
import Amazonka.S3.Sync.Key (BucketKey, Key, Prefix)
import qualified Amazonka.S3.Sync.Key as Key
import Amazonka.S3.Sync.Mocks
import Amazonka.S3.Sync.ObjectAttributes
import Amazonka.S3.Sync.Options
import Amazonka.S3.Sync.PairedItem
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Path (absdir, absfile)
import Test.Hspec

spec :: Spec
spec = do
  describe "syncLocalRemoteM" $ do
    it "works for a simple case" $ do
      let
        src :: Path Abs Dir
        src = [absdir|/src/|]

        pre :: HasCallStack => Key Key.Rel Prefix
        pre = fromTextUnsafe "dst/"

        dst :: BucketKey Key.Abs Prefix
        dst = Key.joinBucketKey (Key.rootBucketKey "bucket") pre

      mocks <-
        Mocks
          <$> mockDir
            [ "/src/foo.txt 123 2024-01-01T12:00:00.0Z"
            , "/src/bar.txt 457 2024-01-02T10:00:00.0Z"
            ]
          <*> mockAWS
            [ "s3://bucket/dst/foo.txt 133 2024-01-01T11:59:00.0Z"
            ]

      let options =
            SyncOptions
              { dryRun = DryRun
              , includeExcludes = []
              , delete = Delete
              , sizeOnly = NotSizeOnly
              }

      actions <- runMocksM (syncLocalRemoteM options src dst) mocks
      actions
        `shouldMatchList` [ CreateObject
                              $ PairedItem
                                { file = [absfile|/src/bar.txt|]
                                , object = fromTextUnsafe "s3://bucket/dst/bar.txt"
                                , details = NoDetails
                                }
                          , UpdateObject
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
                                            { size = 133
                                            , lastModified = makeUTCTime 2024 1 1 11 59
                                            }
                                      }
                                }
                          ]

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
