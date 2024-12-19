{-# OPTIONS_GHC -Wno-orphans #-}

module Amazonka.S3.Sync.Prelude
  ( module X
  , module Amazonka.S3.Sync.Prelude
  ) where

-- "abs" is a common variable for Abs paths and keys
import Prelude as X hiding (abs)

import Amazonka.Data.Text as X (FromText (..), ToText (..))
import Blammo.Logging as X
import Control.Error.Util as X (hush, note)
import Control.Monad as X (filterM, guard, join, unless, void, when, (<=<))
import Control.Monad.AWS as X (MonadAWS)
import Control.Monad.Catch as X (MonadThrow (..))
import Control.Monad.IO.Class as X (MonadIO (..))
import Control.Monad.IO.Unlift as X (MonadUnliftIO)
import Data.Aeson as X (ToJSON)
import Data.Bifunctor as X (bimap, first, second)
import Data.Bitraversable as X (Bitraversable, bimapM)
import Data.Foldable as X (for_, traverse_)
import Data.Function as X ((&))
import Data.Kind as X (Type)
import Data.List.NonEmpty as X (NonEmpty (..))
import Data.Maybe as X (fromMaybe)
import Data.Text as X (Text, pack, unpack)
import Data.These as X
import Data.Time as X (UTCTime)
import Data.Traversable as X (for)
import GHC.Generics as X (Generic)
import GHC.Records as X
import Lens.Micro as X (lens, to, (%~), (.~), (?~), (^.))
import Path as X
  ( Abs
  , Dir
  , File
  , Path
  , Rel
  , SomeBase (..)
  , parseAbsDir
  , parseAbsFile
  , parseRelDir
  , parseRelFile
  , toFilePath
  , (</>)
  )
import System.FilePath.Glob as X (Pattern)

firstM :: (Applicative f, Bitraversable t) => (a -> f a') -> t a b -> f (t a' b)
firstM f = bimapM f pure

secondM
  :: (Applicative f, Bitraversable t) => (b -> f b') -> t a b -> f (t a b')
secondM = bimapM pure

panic :: MonadThrow m => String -> m a
panic msg =
  throwM
    $ userError
    $ "panic! "
      <> msg
      <> ".\nPlease report this at https://github.com/pbrisbin/amazonka-contrib-s3-sync/issues"

instance ToText (Path b t) where
  toText = pack . toFilePath
