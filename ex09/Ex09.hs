-- Ex9: Paint Calculator
--
-- - Prompt for room length and width.
-- - Compute area and divide by 350 (coverage per gallon).
-- - Round up to the next whole gallon.
-- - Use a constant for coverage rate.-- Ex8: Pizza PeopleAndPizza

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs
           , FlexibleContexts, TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <&>" #-}

 module Ex09 where

import Data.Functor ((<&>))
import Data.String.Interpolate (i)
import Data.Text (Text, unpack)
import Text.Read (readMaybe)
import Polysemy (Sem, makeSem, Member)

data Console m r where
  ReadLine  :: Text -> Console m Text
  WriteLine :: Text -> Console m ()

makeSem ''Console

readNonNegative :: Member Console r => Text -> Sem r Int
readNonNegative prompt =
  readLine prompt <&> (readMaybe . unpack) >>= \case
    Just n | n >= 0 -> return n
    _               -> writeLine "Invalid input."
                    >> readNonNegative prompt

program :: Member Console r => Sem r ()
program = do
  l <- readNonNegative "Enter the length of the room in feet: "
  w <- readNonNegative "Enter the width of the room in feet: "
  let area = l * w
      gallons = ceiling (fromIntegral area / 350.0 :: Double) :: Int
  writeLine [i|You will need to purchase #{gallons} gallons of|]
  writeLine [i|paint to cover #{area} square feet.|]
