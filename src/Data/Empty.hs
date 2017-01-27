{- |

Module      :  Data.Empty
Copyright   :  Vadim Vinnik, 2017
License     :  BSD3

Maintainer  :  vadim.vinnik@gmail.com
Stability   :  experimental
Portability :  portable

An always empty data structure, i.e. a degenerate container that contains no
elements but is aware of the (missing) elements' type,  and is a first-class
citizen of a number of type classes.  E.g. 'fmap'ping of any function over
'Empty' is also 'Empty', two 'Empty' container can be 'mappend'ed that gives
'Empty' again etc.

-}

module Data.Empty (
  Empty(..),
  unwrap
) where

import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..))

-- | the empty container
data Empty a = Empty

-- | obtain the (missing) value from the container -- always fails
unwrap :: Empty a -> a
unwrap = undefined -- not a stub but the desired correct implementation

-- for internal use: creates a const function of 2 arguments
const2 :: a -> b -> c -> a
const2 = const . const

instance Eq (Empty a) where
  (==) = const2 True

instance Ord (Empty a) where
  compare = const2 EQ

instance Monoid (Empty a) where
  mempty = Empty
  mappend = const2 Empty

instance Foldable Empty where
  foldr _ = const
  foldl _ = const
  null = const False
  length = const 0
  elem = const2 False

instance Functor Empty where
  fmap _ e = Empty

instance Traversable Empty where
  traverse _ Empty = pure Empty

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
