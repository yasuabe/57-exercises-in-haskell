-- Ex14: Tax Calculator
--
-- ・ Prompt the user for the order amount and the state.
-- ・ If the state is "WI", calculate 5.5% tax and display subtotal, tax, and total.
-- ・ For other states, display only the total.
-- ・ Use only a simple if statement (no else clause).
-- ・ Round all money up to the nearest cent.
-- ・ Use a single output statement at the end.

module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Text.Regex.TDFA ((=~))
import Common.Util (readWithPrompt)

type AppType a = ExceptT String IO a

data USState = WI | Other deriving (Show)

instance Read USState where
  readsPrec _ input
    | input == "WI"     = [(WI, "")]
    | length input == 2 = [(Other, "")]
    | otherwise         = []

newtype Cents = Cents Integer deriving (Show)

instance Read Cents where
  readsPrec _ input = 
    if input =~ "^\\s*[0-9]+(\\.[0-9]{1,2})?\\s*$"
      then case (readMaybe input :: Maybe Double) of
        Just x -> [(Cents (floor (x * 100.0)), "")]
        _      -> []
      else []

readCents :: String -> AppType Cents
readCents prompt = readWithPrompt prompt "Invalid amount"

program :: AppType ()
program = do
  amount  <- readCents "What is the order amount? "
  usState <- readState "What is the state? "
  liftIO $ putStrLn $ formatOutput amount usState
  where
    readState prompt = readWithPrompt prompt "Provide a valid state code (2 characters)."
    formatOutput :: Cents -> USState -> String
    formatOutput (Cents cents) usState = 
      let amount = fromIntegral cents / 100.0 :: Double
      in case usState of
        WI -> let tax   = amount * 0.055
                  total = amount + tax
              in unlines [ printf "Subtotal: $%.2f" amount
                         , printf "Tax: $%.2f"      tax
                         , printf "Total: $%.2f"    total
                         ]
        Other -> printf "Total: $%.2f" amount

main :: IO ()
main = runExceptT program >>= either (putStrLn . ("Error: " ++)) (const (return ()))
