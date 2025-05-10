{-# LANGUAGE DeriveGeneric #-}
module Ex40  where

import GHC.Generics (Generic)

newtype MaxTriple = MaxTriple (Int, Int, Int)
  deriving (Eq, Show, Generic)

instance Semigroup MaxTriple where
  MaxTriple (a, b, c) <> MaxTriple (x, y, z) =
    MaxTriple (max a x, max b y, max c z)

instance Monoid MaxTriple where
  mempty = MaxTriple (minBound, minBound, minBound)
