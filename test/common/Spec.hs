module Spec where

import Data.Proxy (Proxy(..))
import GHC.Generics (Generic)
import Test.QuickCheck
import Test.QuickCheck.Classes.Base (monoidLaws, functorLaws)
import Test.QuickCheck.Classes (lawsCheckMany)

import Common.Tuples (MaxPair(..), Pair(..))

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary

instance Arbitrary MaxPair where
  arbitrary = MaxPair <$> arbitrary

main :: IO ()
main = lawsCheckMany
  [ ("Monoid MaxPair", [monoidLaws (Proxy :: Proxy MaxPair)])
  , ("Functor Pair", [functorLaws (Proxy :: Proxy Pair)])
  ]
