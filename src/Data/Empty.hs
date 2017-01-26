module Data.Empty (
  Empty,
  unwrap
) where

import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..))

data Empty a = Empty

unwrap :: Empty a -> a
-- not a temporary placeholder, this is the desired and correct implementation
unwrap = undefined

const2 :: a -> b -> c -> a
const2 = const . const

instance Eq (Empty a) where
  (==) = const2 True

instance Ord (Empty a) where
  compare = const2 EQ

instance Monoid (Empty a) where
  mempty = Empty
  mappend = const2 Empty

instance Functor Empty where
  fmap _ e = Empty

instance Applicative Empty where
  pure = const Empty
  (<*>) = const2 Empty

instance Alternative Empty where
  empty = Empty
  (<|>) = const2 Empty

instance Monad Empty where
  (>>=) = const2 Empty

instance MonadPlus Empty where
  mplus = const2 Empty
