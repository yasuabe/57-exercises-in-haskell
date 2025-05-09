module Spec where

import Data.Text as T ( Text, pack, unpack, length )
import Test.QuickCheck ( quickCheck, Property, forAll, Gen, elements, vectorOf, (==>), shuffle )
import Data.Function ((&))
import Control.Applicative (liftA2)
import Ex24 ( isAnagram )

genCustomText :: Gen Text
genCustomText = do
  count <- elements [0, 1, 2, 3]
  chars <- vectorOf count (elements ['a', 'A', 'b', 'B'])
  return $ T.pack chars

genCustomTextPair :: Gen (Text, Text)
genCustomTextPair = liftA2 (,) genCustomText genCustomText

prop_reflexivity :: Property
prop_reflexivity = forAll genCustomText $ \text -> isAnagram text text

prop_shufflePreservesAnagram :: Property
prop_shufflePreservesAnagram = forAll genCustomText $ \text -> do
  text' <- shuffle (unpack text) & fmap pack 
  return $ isAnagram text text'

prop_commutative :: Property
prop_commutative = forAll genCustomTextPair $ \(first, second) ->
  isAnagram first second == isAnagram second first

prop_lengthMismatch :: Property
prop_lengthMismatch = forAll genCustomTextPair $ \(first, second) ->
  T.length first /= T.length second ==> not (isAnagram first second)

main :: IO ()
main = do
  quickCheck prop_reflexivity
  quickCheck prop_shufflePreservesAnagram
  quickCheck prop_commutative
  quickCheck prop_lengthMismatch

