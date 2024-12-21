{-# LANGUAGE DerivingVia #-}

-- | MTL machinery for mocking SyncSpec
module Amazonka.S3.Sync.Mocks
  ( Mocks (..)
  , mockDir
  , mockAWS
  , MocksM
  , runMocksM
  ) where

import Amazonka.S3.Sync.Prelude

import qualified Amazonka.S3.ListObjectsV2 as S3
import Amazonka.S3.Sync.FileDetails
import Amazonka.S3.Sync.Key (BucketKey, Object, toObjectKey)
import qualified Amazonka.S3.Sync.Key as Key
import Amazonka.S3.Sync.ObjectAttributes
import qualified Amazonka.S3.Types.Object as S3
import qualified Amazonka.S3.Types.ObjectStorageClass as S3
import Control.Monad.AWS.Matchers
import Control.Monad.AWS.ViaMock
import Control.Monad.Directory
import Control.Monad.Output
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks, runReader)
import Data.Char (isDigit)
import Data.List.NonEmpty (nonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Time.Format.ISO8601 (formatReadP, iso8601Format)
import Test.Hspec
import Text.ParserCombinators.ReadP
import qualified Prelude as Unsafe (read)

-- import Data.List (nub)
-- import Path (parent, stripProperPrefix)

newtype MocksM a = MocksM
  { unwrap :: ReaderT Mocks IO a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadThrow
    , MonadReader Mocks
    )
  deriving (MonadAWS) via (MockAWS MocksM)

instance MonadOutput MocksM where
  puts _ = pure ()

instance MonadDirectory MocksM where
  -- listDir d = asks $ first nub . foldMap go . Map.keys . (.dir)
  --  where
  --   go :: Path Abs File -> ([Path Abs Dir], [Path Abs File])
  --   go file
  --     | parent file == d = ([], [file])
  --     | Just _ <- stripProperPrefix d file = error "TODO"
  --     | otherwise = ([], [])

  listDir = undefined
  listDirRel = undefined
  doesFileExist f = asks $ Map.member (toFilePath f) . (.dir)
  getFileSize = getFileDetail (.size)
  getModificationTime = getFileDetail (.mtime)

getFileDetail
  :: MonadReader Mocks m
  => (FileDetails -> a)
  -> Path b File
  -> m a
getFileDetail attr f = asks $ maybe err attr . Map.lookup (toFilePath f) . (.dir)
 where
  err =
    error
      $ "operation attempted on "
        <> toFilePath f
        <> ", which is not present in mocks"

runMocksM :: MocksM a -> Mocks -> IO a
runMocksM f = runReaderT f.unwrap

data Mocks = Mocks
  { dir :: Map FilePath FileDetails
  , aws :: Matchers
  }

instance HasMatchers Mocks where
  matchersL = lens (.aws) $ \x y -> x {aws = y}

mockDir :: [String] -> IO (Map FilePath FileDetails)
mockDir = fmap Map.fromList . traverse (parse dirP)

mockAWS :: [String] -> IO Matchers
mockAWS lns = do
  ks <- traverse (parse awsP) lns
  pure $ mkMatchers [toListMatcher ks]

toListMatcher :: [(BucketKey Key.Abs Object, ObjectAttributes)] -> Matcher
toListMatcher ks =
  SendMatcher matches
    $ Right
    $ S3.newListObjectsV2Response 200
      & S3.listObjectsV2Response_contents
      ?~ map (uncurry toS3Object) ks
 where
  matches :: S3.ListObjectsV2 -> Bool
  matches _ = True -- TODO: bucket + prefix
  toS3Object :: BucketKey Key.Abs Object -> ObjectAttributes -> S3.Object
  toS3Object bk attrs =
    let
      relKey =
        maybe (toObjectKey bk.key) toObjectKey
          $ Key.stripProperPrefix Key.rootKey bk.key
      sClass = S3.ObjectStorageClass_STANDARD
    in
      S3.newObject "etag" attrs.size relKey sClass attrs.lastModified

parse :: ReadP a -> String -> IO a
parse p s =
  maybe err (pure . fst . NE.last)
    $ nonEmpty
    $ readP_to_S (p <* eof) s
 where
  err :: IO a
  err = do
    expectationFailure $ "The line " <> s <> " failed to parse"
    error "unreachable"

dirP :: ReadP (FilePath, FileDetails)
dirP =
  (,)
    <$> (pathP <* char ' ' <* skipSpaces)
    <*> fileDetailsP

pathP :: ReadP FilePath
pathP = do
  s <- many1 anyChar
  toFilePath <$> eitherParser parseAbsFile s

fileDetailsP :: ReadP FileDetails
fileDetailsP =
  FileDetails
    <$> (sizeP <* char ' ' <* skipSpaces)
    <*> utcTimeP

awsP :: ReadP (BucketKey Key.Abs Object, ObjectAttributes)
awsP =
  (,)
    <$> (bucketKeyP <* char ' ' <* skipSpaces)
    <*> objectAttributesP

bucketKeyP :: ReadP (BucketKey Key.Abs Object)
bucketKeyP = do
  s <- many1 anyChar
  eitherParser (fromText . pack) s

objectAttributesP :: ReadP ObjectAttributes
objectAttributesP =
  ObjectAttributes
    <$> (sizeP <* char ' ' <* skipSpaces)
    <*> utcTimeP

sizeP :: ReadP Integer
sizeP = do
  x <- many1 $ satisfy isDigit
  pure $ Unsafe.read x

utcTimeP :: ReadP UTCTime
utcTimeP = formatReadP iso8601Format

eitherParser :: (String -> Either e a) -> String -> ReadP a
eitherParser f s = case f s of
  Left {} -> pfail
  Right a -> pure a

anyChar :: ReadP Char
anyChar = satisfy $ const True

-- Ugh, upstream gives us no way to make Matchers
mkMatchers :: [Matcher] -> Matchers
mkMatchers ms = runReader (withMatchers ms ask) mempty
