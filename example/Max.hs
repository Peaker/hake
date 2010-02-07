module Max(Max(..), AddBounds(..)) where

import Data.Monoid(Monoid(..))
import Control.Applicative(Applicative(..), liftA2)

data AddBounds a = MinBound | Unbound a | MaxBound
  deriving (Read, Show, Eq, Ord)

instance Bounded (AddBounds a) where
  minBound = MinBound
  maxBound = MaxBound

instance Functor AddBounds where
  fmap f (Unbound x) = Unbound (f x)
  fmap _ MinBound = MinBound
  fmap _ MaxBound = MaxBound

newtype Max a = Max { unMax :: a }
  deriving (Read, Show, Eq, Ord, Bounded)

instance Functor Max where
  fmap f = Max . f . unMax

instance Applicative Max where
  pure = Max
  Max f <*> Max x = Max (f x)

instance Monad Max where
  return = pure
  Max x >>= f = f x

instance (Ord a, Bounded a) => Monoid (Max a) where
  mempty = pure minBound
  mappend = liftA2 max
