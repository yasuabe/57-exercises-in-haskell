module Spec where

import Data.Proxy (Proxy(..))
import GHC.Generics (Generic)
import Test.QuickCheck
import Test.QuickCheck.Classes.Base (monoidLaws)
import Test.QuickCheck.Classes (lawsCheckMany)

import Ex40 (MaxTriple(..))

instance Arbitrary MaxTriple where
  arbitrary = MaxTriple <$> arbitrary

main :: IO ()
main = lawsCheckMany
  [ ("Monoid MaxTriple", [monoidLaws (Proxy :: Proxy MaxTriple)]) ]
