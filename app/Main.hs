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
  go
    <$> argumentParser
    <*> argumentParser
 where
  go = curry $ \case
    (Left d, Right p) -> SyncTo d p
    (Right p, Left d) -> SyncFrom p d
    (Right a, Right b) -> SyncBetween a b
    _ -> error "amazonka-sync-s3 cannot be used with LocalPath LocalPath (try rsync)"

argumentParser :: Parser (Either (Path Rel Dir) (BucketKey Abs Prefix))
argumentParser = argument (eitherReader argumentReader) (metavar "LocalPath|S3Uri")

argumentReader
  :: String -> Either String (Either (Path Rel Dir) (BucketKey Abs Prefix))
argumentReader s =
  case (first show $ parseRelDir s, fromText $ pack s) of
    (_, Right p) -> Right (Right p)
    (Right d, _) -> Right (Left d)
    (Left a, Left b) ->
      Left
        $ "argument did not parse as LocalPath or S3Uri: "
          <> "\n as LocalPath: "
          <> a
          <> "\n as S3URI: "
          <> b
