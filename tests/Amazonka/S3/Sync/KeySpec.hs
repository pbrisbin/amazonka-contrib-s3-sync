{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Amazonka.S3.Sync.KeySpec
  ( spec
  ) where

import Amazonka.S3.Sync.Prelude

import Amazonka.S3.Sync.Key
import Data.Either (isLeft)
import Data.Maybe (isJust)
import Test.Hspec

spec :: Spec
spec = do
  context "Key" $ do
    describe "rootKey" $ do
      it "is /" $ do
        expected <- parseAbsPrefix "/"
        rootKey `shouldBe` expected

    describe "joinKey" $ do
      it "joins a prefix and object" $ do
        abs <- parseAbsPrefix "/foo/bar/"
        rel <- parseRelObject "baz/bat.txt"
        expected <- parseAbsObject "/foo/bar/baz/bat.txt"
        joinKey abs rel `shouldBe` expected

    context "prefixes" $ do
      describe "stripProperPrefix" $ do
        it "works with root" $ do
          abs <- parseAbsPrefix "/foo/bar/"
          rel <- parseRelPrefix "foo/bar/"
          stripProperPrefix rootKey abs `shouldReturn` rel

        it "works with prefixes" $ do
          key <- parseAbsPrefix "/foo/"
          abs <- parseAbsPrefix "/foo/bar/"
          rel <- parseRelPrefix "bar/"
          stripProperPrefix key abs `shouldReturn` rel

        it "works with objects" $ do
          key <- parseAbsPrefix "/foo/"
          abs <- parseAbsObject "/foo/bar/baz.txt"
          rel <- parseRelObject "bar/baz.txt"
          stripProperPrefix key abs `shouldReturn` rel

      describe "replaceProperPrefix" $ do
        it "works with both absolute" $ do
          abs1 <- parseAbsPrefix "/foo/bar/"
          abs2 <- parseAbsPrefix "/baz/"
          input <- parseAbsObject "/foo/bar/bat.txt"
          expected <- parseAbsObject "/baz/bat.txt"
          replaceProperPrefix abs1 abs2 input `shouldReturn` expected

        it "works with both relative" $ do
          rel1 <- parseRelPrefix "foo/bar/"
          rel2 <- parseRelPrefix "baz/"
          input <- parseRelObject "foo/bar/bat.txt"
          expected <- parseRelObject "baz/bat.txt"
          replaceProperPrefix rel1 rel2 input `shouldReturn` expected

        it "works with absolute to relative" $ do
          abs <- parseAbsPrefix "/foo/bar/"
          rel <- parseRelPrefix "baz/"
          input <- parseAbsObject "/foo/bar/bat.txt"
          expected <- parseRelObject "baz/bat.txt"
          replaceProperPrefix abs rel input `shouldReturn` expected

        it "works with relative to absolute" $ do
          rel <- parseRelPrefix "foo/bar/"
          abs <- parseAbsPrefix "/baz/"
          input <- parseRelObject "foo/bar/bat.txt"
          expected <- parseAbsObject "/baz/bat.txt"
          replaceProperPrefix rel abs input `shouldReturn` expected

    context "parsing" $ do
      describe "parseAbsPrefix" $ do
        -- AbsPrefix
        it "works" $ do
          parseAbsPrefix "/foo/bar/" `shouldSatisfy` isJust

        -- Abs
        it "rejects missing leading slash" $ do
          parseAbsPrefix "foo/bar/" `shouldBe` Nothing

        -- Prefix
        it "rejects missing trailing slash" $ do
          parseAbsPrefix "/foo/bar" `shouldBe` Nothing

      describe "parseRelPrefix" $ do
        -- RelPrefix
        it "works" $ do
          parseRelPrefix "foo/bar/" `shouldSatisfy` isJust

        -- Rel
        it "rejects present leading slash" $ do
          parseRelPrefix "/foo/bar/" `shouldBe` Nothing

        -- Prefix
        it "rejects missing trailing slash" $ do
          parseRelPrefix "foo/bar" `shouldBe` Nothing

      describe "parseAbsObject" $ do
        -- AbsObject
        it "works" $ do
          parseAbsObject "/foo/bar.txt" `shouldSatisfy` isJust

        -- Abs
        it "rejects missing leading slash" $ do
          parseAbsObject "foo/bar.txt" `shouldBe` Nothing

        -- Object
        it "rejects present trailing slash" $ do
          parseAbsObject "/foo/bar.txt/" `shouldBe` Nothing

      describe "parseRelObject" $ do
        -- RelObject
        it "works" $ do
          parseRelObject "foo/bar.txt" `shouldSatisfy` isJust

        -- Rel
        it "rejects present leading slash" $ do
          parseRelObject "/foo/bar.txt" `shouldBe` Nothing

        -- Object
        it "rejects present trailing slash" $ do
          parseRelObject "foo/bar.txt/" `shouldBe` Nothing

    context "conversion to ObjectKey" $ do
      let cases =
            [
              ( "fromAbsPrefix . parseAbsPrefix"
              , fmap fromAbsPrefix . parseAbsPrefix
              , "/foo/bar/"
              )
            ,
              ( "fromRelPrefix . parseRelPrefix"
              , fmap fromRelPrefix . parseRelPrefix
              , "foo/bar/"
              )
            ,
              ( "fromAbsObject . parseAbsObject"
              , fmap fromAbsObject . parseAbsObject
              , "/foo/bar.txt"
              )
            ,
              ( "fromRelObject . parseRelObject"
              , fmap fromRelObject . parseRelObject
              , "foo/bar.txt"
              )
            ,
              ( "toObjectKey . parseAbsPrefix"
              , fmap toObjectKey . parseAbsPrefix
              , "/foo/bar/"
              )
            ,
              ( "toObjectKey . parseRelPrefix"
              , fmap toObjectKey . parseRelPrefix
              , "foo/bar/"
              )
            ,
              ( "toObjectKey . parseAbsObject"
              , fmap toObjectKey . parseAbsObject
              , "/foo/bar.txt"
              )
            ,
              ( "toObjectKey . parseRelObject"
              , fmap toObjectKey . parseRelObject
              , "foo/bar.txt"
              )
            ]

      for_ cases $ \(name, parseRender, input) -> do
        describe name $ do
          it "round-trips" $ do
            x <- parseRender input
            x `shouldBe` input

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
