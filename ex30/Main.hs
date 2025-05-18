{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- Ex30: Multiplication Table
-- 
-- ・Print a full multiplication table from 0×0 to 12×12.
-- ・Format each line as a x b = c.
-- ・Constraint: Use nested loops to implement the logic.

module Main where

import qualified Streamly.Data.Stream as S
import qualified Streamly.Data.Fold as F
import qualified Streamly.Internal.Data.Unfold as UI

import Data.String.Interpolate (i)

crossProduct :: (Monad m) => (Int, Int) -> S.Stream m (Int, Int)
crossProduct = S.unfold
             $ UI.crossWithM (curry return)
                             UI.enumerateFromTo
                             UI.enumerateFromTo

main :: IO ()
main = S.fold F.drain
     $ S.mapM printMult
     $ crossProduct (0, 12)
  where
    printMult :: (Int, Int) -> IO ()
    printMult (x, y) = putStrLn [i|#{x} x #{y} = #{x * y}|]
