module Amazonka.S3.Sync.PairUp
  ( pairUpM
  , pairUp
  ) where

import Amazonka.S3.Sync.Prelude

import Data.Functor.Identity (runIdentity)

pairUpM :: Monad m => (a -> b -> m Ordering) -> [a] -> [b] -> m [These a b]
pairUpM _ as [] = pure $ This <$> as
pairUpM _ [] bs = pure $ That <$> bs
pairUpM cmp (a : as) (b : bs) = do
  o <- cmp a b

  case o of
    EQ -> (These a b :) <$> pairUpM cmp as bs
    LT -> (This a :) <$> pairUpM cmp as (b : bs)
    GT -> (That b :) <$> pairUpM cmp (a : as) bs

-- | Pure version, for testing
pairUp :: (a -> b -> Ordering) -> [a] -> [b] -> [These a b]
pairUp cmp as bs = runIdentity $ pairUpM (\a b -> pure $ cmp a b) as bs
