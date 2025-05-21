{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- Ex10: Self-Checkout
--
-- ・Prompt for price and quantity of 3 items.
-- ・Calculate subtotal, then 5.5% tax, then total.
-- ・Print each line item, subtotal, tax, and total.
-- ・Separate input, processing, and output logic.
-- ・Ensure all input is converted to numeric types before calculations.

module Main where

import Prelude hiding (length)
import Data.Functor ((<&>))
import Data.String.Interpolate (i)
import Data.Text (Text)
import System.Console.Haskeline (InputT)
import Text.Printf (printf)

import Common.App (runProgram)
import Common.System (putTextLn, repeatUntilValid, readInt, toNonNega2Decimals)

type AppType a = InputT IO a
type Cent      = Int
type Quantity  = Int
type Receipt   = (Double, Double, Double) -- Subtotal, tax, total

readPrice :: Text -> AppType Cent
readPrice prompt = repeatUntilValid toNonNega2Decimals prompt "Please enter a valid price."
               <&> (round . (* 100.0)) -- Convert to cents

readQuantity :: Text -> AppType Quantity
readQuantity prompt = repeatUntilValid readInt prompt "Please enter a valid quantity."

readItem :: Int -> AppType (Cent, Quantity)
readItem n = (,)
         <$> readPrice [i|Enter the price of item #{n}: |]
         <*> readQuantity [i|Enter the quantity of item #{n}: |]

program :: AppType ()
program = do
    items <- mapM readItem [1..3]
    let receipt = process items
    displayItems receipt 
  where
    process :: [(Cent, Quantity)] -> Receipt
    process items =
      let
        subtotal = sum [fromIntegral p * fromIntegral q| (p, q) <- items]
        tax      = subtotal * 0.055
        total    = subtotal + tax
      in (subtotal, tax, total)

    showCent :: Double -> String -- Convert cents (as Double) to a dollar string
    showCent c = [i|$#{printf "%.2f" $ c / 100.0::String}|]

    displayItems :: Receipt -> AppType ()
    displayItems (subtotal, tax, total) = do
      putTextLn [i|Subtotal: #{showCent subtotal}|]
      putTextLn [i|Tax: #{showCent tax}|]
      putTextLn [i|Total: #{showCent total}|]

main :: IO ()
main = runProgram program
