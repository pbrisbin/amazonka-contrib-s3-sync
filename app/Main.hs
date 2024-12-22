module Main
  ( main
  ) where

import Amazonka.S3.Sync.Prelude

import qualified Amazonka
import Amazonka.S3.Sync
import Amazonka.S3.Sync.Key
import Amazonka.S3.Sync.Options
import Amazonka.S3.Sync.Options.IncludeExclude
import Options.Applicative

main :: IO ()
main = do
  opt <- parseOptions
  env <- Amazonka.newEnv Amazonka.discover
  sync env opt

parseOptions :: IO SyncOptions
parseOptions =
  execParser
    $ info (optionsParser <**> helper)
    $ fullDesc <> progDesc "sync directories and S3 prefixes"

optionsParser :: Parser SyncOptions
optionsParser =
  SyncOptions
    <$> flag NotDryRun DryRun (long "dryrun")
    <*> includeExcludesParser
    <*> flag Don'tDelete Delete (long "delete")
    <*> flag NotSizeOnly SizeOnly (long "size-only")
    <*> argumentsParser

includeExcludesParser :: Parser [IncludeExclude]
includeExcludesParser =
  many
    $ asum
      [ Include <$> strOption (long "include")
      , Exclude <$> strOption (long "exclude")
      ]

argumentsParser :: Parser SyncArguments
argumentsParser =
  syncToParser
    <|> syncFromParser
    <|> syncBetweenParser
    <|> failParser "TODO"

syncToParser :: Parser SyncArguments
syncToParser =
  SyncTo
    <$> relDirParser
    <*> bucketPrefixParser

syncFromParser :: Parser SyncArguments
syncFromParser =
  SyncFrom
    <$> bucketPrefixParser
    <*> relDirParser

syncBetweenParser :: Parser SyncArguments
syncBetweenParser =
  SyncBetween
    <$> bucketPrefixParser
    <*> bucketPrefixParser

relDirParser :: Parser (Path Rel Dir)
relDirParser = argument (eitherReader $ first show . parseRelDir) (metavar "LocalPath")

bucketPrefixParser :: Parser (BucketKey Abs Prefix)
bucketPrefixParser = argument (eitherReader $ fromText . pack) (metavar "S3Uri")

failParser :: String -> Parser a
failParser msg = error "unreachable" <$> infoOption msg mempty
