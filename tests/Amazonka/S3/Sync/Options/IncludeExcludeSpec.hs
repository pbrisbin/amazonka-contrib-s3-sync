{-# LANGUAGE QuasiQuotes #-}

module Amazonka.S3.Sync.Options.IncludeExcludeSpec
  ( spec
  ) where

import Amazonka.S3.Sync.Prelude

import Amazonka.S3.Sync.Options.IncludeExclude
import Path (relfile)
import Test.Hspec

spec :: Spec
spec = do
  describe "shouldIncludePath" $ do
    it "handles include-then-exclude" $ do
      let ies = [Include "*", Exclude "*.txt", Exclude "*.tmp"]

      shouldIncludePath [relfile|foo.md|] ies `shouldBe` True
      shouldIncludePath [relfile|foo.txt|] ies `shouldBe` False
      shouldIncludePath [relfile|foo.tmp|] ies `shouldBe` False

    it "handles exclude-then-include" $ do
      let ies = [Exclude "*", Include "*.md", Include "*.txt"]

      shouldIncludePath [relfile|foo.md|] ies `shouldBe` True
      shouldIncludePath [relfile|foo.txt|] ies `shouldBe` True
      shouldIncludePath [relfile|foo.tmp|] ies `shouldBe` False
