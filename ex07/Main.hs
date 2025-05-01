{-# LANGUAGE QuasiQuotes #-}
-- Ex7: Area of a Rectangular Room
--
-- - Prompt the user to enter the length and width of a room in feet.
-- - Calculate the area in square feet.
-- - Convert the area to square meters using a constant conversion factor.
-- - Keep calculations separate from output.
-- - Display both square feet and square meters in the output.
module Main where

import Control.Monad.Trans.Except ( runExceptT, ExceptT )
import Control.Monad.IO.Class (liftIO)
import Data.String.Interpolate (i)
import Prelude hiding (length)
import Common.Util (readDouble)
import Ex07 (toSquareMeter)

type AppType a = ExceptT String IO a

displayOutput :: Double -> Double -> Double -> Double -> AppType ()
displayOutput length width squareFeet squareMeters = liftIO printAll
  where
    to3Digits :: Double -> Double
    to3Digits x = fromIntegral (round (x * 1000.0) :: Integer) / 1000.0
    printAll = do
      putStrLn [i|You entered dimensions of #{width} feet by #{length} feet.|]
      putStrLn "The area is"
      putStrLn [i|#{to3Digits squareFeet} square feet|]
      putStrLn [i|#{to3Digits squareMeters} square meters|]

program :: AppType ()
program = do
  length <- readDimension "length"
  width  <- readDimension "width"

  let squareFeet   = length * width
  let squareMeters = toSquareMeter squareFeet

  displayOutput length width squareFeet squareMeters
  where
    readDimension :: String -> AppType Double
    readDimension d = readDouble [i|What is the #{d} of the room in feet? |]

main :: IO ()
main = runExceptT program >>= either (putStrLn . ("Error: " ++)) (const (return ()))
