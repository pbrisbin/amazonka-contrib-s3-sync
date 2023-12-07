module Amazonka.S3.Sync.ObjectAttributes
  ( ObjectAttributes (..)
  , getObjectAttributes
  )
where

import Amazonka.S3.Sync.Prelude

import Amazonka.S3.Types (object_lastModified, object_size)
import qualified Amazonka.S3.Types as S3

data ObjectAttributes = ObjectAttributes
  { size :: Integer
  , lastModified :: UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

getObjectAttributes :: S3.Object -> ObjectAttributes
getObjectAttributes obj =
  ObjectAttributes
    { size = obj ^. object_size
    , lastModified = obj ^. object_lastModified
    }
