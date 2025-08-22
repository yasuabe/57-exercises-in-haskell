{-# LANGUAGE QuasiQuotes #-}

module Spec where
import Ex03
import Test.QuickCheck
import Data.String.Interpolate (i)

prop_makeOutput:: String -> String -> Bool
prop_makeOutput author quote =
  [i|#{author} says, "#{quote}"|] == makeOutput author quote

main :: IO ()
main = quickCheck prop_makeOutput
