{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Amazonka.S3.Sync.KeySpec
  ( spec
  ) where

import Amazonka.S3.Sync.Prelude

import Amazonka.S3.Sync.Key
import Data.Either (isLeft)
import Test.Hspec

spec :: Spec
spec = do
  context "Key" $ do
    describe "rootKey" $ pure ()
    describe "joinKey" $ pure ()

    context "prefixes" $ do
      describe "stripProperPrefix" $ pure ()
      describe "replaceProperPrefix" $ pure ()
      describe "dropRoot" $ pure ()
    context "parsing" $ do
      describe "parseAbsPrefix" $ pure ()
      describe "parseRelPrefix" $ pure ()
      describe "parseAbsObject" $ pure ()
      describe "parseRelObject" $ pure ()

    context "conversion to ObjectKey" $ do
      describe "toObjectKey" $ pure ()
      describe "fromAbsPrefix" $ pure ()
      describe "fromRelPrefix" $ pure ()
      describe "fromAbsObject" $ pure ()
      describe "fromRelObject" $ pure ()

  context "BucketKey" $ do
    context "parsing" $ do
      describe "fromText" $ do
        it "parses without slash as an empty prefix" $ do
          fromText @(BucketKey Abs Prefix) "s3://some-bucket"
            `shouldSatisfy` \case
              Left {} -> False
              Right bk -> bk.bucket == "some-bucket" && bk.key == rootKey

        it "parses without prefix" $ do
          fromText @(BucketKey Abs Prefix) "s3://some-bucket/"
            `shouldSatisfy` \case
              Left {} -> False
              Right bk -> bk.bucket == "some-bucket" && bk.key == rootKey

        it "parses with prefix" $ do
          let Right prefix = parseAbsPrefix "/some-prefix/"

          fromText @(BucketKey Abs Prefix) "s3://some-bucket/some-prefix/"
            `shouldSatisfy` \case
              Left {} -> False
              Right bk -> bk.bucket == "some-bucket" && bk.key == prefix

        it "validates s3://" $ do
          fromText @(BucketKey Abs Prefix) "http://" `shouldSatisfy` isLeft

      it "validates bucket presence" $ do
        fromText @(BucketKey Abs Prefix) "s3://" `shouldSatisfy` isLeft

      it "validates bucket validity" $ do
        fromText @(BucketKey Abs Prefix) "s3:/b$$/" `shouldSatisfy` isLeft

      it "validates prefix" $ do
        fromText @(BucketKey Abs Prefix) "s3:/some-bucket/some-object"
          `shouldSatisfy` isLeft

      describe "rootBucketKey" $ pure ()
      describe "joinBucketKey" $ pure ()
