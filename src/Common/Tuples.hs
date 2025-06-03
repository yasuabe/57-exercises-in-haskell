{-# LANGUAGE DeriveGeneric #-}

module Common.Tuples where

import GHC.Generics (Generic)

newtype MaxPair = MaxPair (Int, Int)
  deriving (Eq, Show, Generic)

instance Semigroup MaxPair where
  MaxPair (a, b) <> MaxPair (x, y) =
    MaxPair (max a x, max b y)

instance Monoid MaxPair where
  mempty = MaxPair (minBound, minBound)

---- Pair
newtype Pair a = Pair (a, a)
  deriving (Eq, Show, Generic)

instance Functor Pair where
  fmap f (Pair (x, y)) = Pair (f x, f y)