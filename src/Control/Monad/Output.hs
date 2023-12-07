module Control.Monad.Output
  ( MonadOutput (..)
  ) where

import Prelude

import Data.Text (Text)

class Monad m => MonadOutput m where
  puts :: Text -> m ()
