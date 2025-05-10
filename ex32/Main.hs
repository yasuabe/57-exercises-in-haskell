{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

-- Ex32 Guess the Number Game
-- ・Guess the Number with 3 difficulty levels:
--   ・Level 1: 1–10
--   ・Level 2: 1–100
--   ・Level 3: 1–1000
-- ・Random number is picked based on difficulty.
-- ・Player guesses until correct.
-- ・After each guess, show “too high” or “too low”.
-- ・Count and display total guesses.
-- ・Ask to play again after winning.
-- ・Non-numeric input is invalid and counts as a wrong guess.

module Main where

import Control.Monad.Writer.Class (tell)
import Control.Monad.RWS ( RWST, lift, execRWST )
import Control.Monad.RWS.Lazy (ask)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Monoid (Sum(..))
import Data.String.Interpolate (i)
import Data.Text (Text, unpack)
import System.Console.Haskeline (InputT)
import Text.Read (readMaybe)

import Common.App (runProgram)
import Common.System (YesNo(..), askYesNo, readLine, putText, putTextLn)
import Common.Util (randomPositive)

type App = InputT IO

type Session = RWST Int (Sum Int) () App

evaluateGuess :: Int -> Text -> Maybe Ordering
evaluateGuess answer input =
  readMaybe (unpack input) <&> (`compare` answer)

sessionLoop :: Session ()
sessionLoop = do
  answer <- ask
  input  <- readLine "Enter a number: " & lift
  tell 1
  evaluateGuess answer input & \case
    Just LT -> proceed "Too low. Guess again: "
    Just GT -> proceed "Too High. Guess again: "
    Just EQ -> return ()
    Nothing -> proceed "Invalid. Guess again: "
  where
    proceed :: Text -> Session ()
    proceed reply = putText reply & lift >> sessionLoop

readLevel :: App Int
readLevel = tryReadLevel >>= \case
  Just n | n >= 1 && n <= 3 -> return $ 10 ^ n
  _                         -> do
    putTextLn "Invalid choice. Please try again."
    readLevel
  where
    tryReadLevel :: App (Maybe Int)
    tryReadLevel =   readLine "Pick a difficulty level (1, 2, or 3): "
                 <&> readMaybe . unpack

programLoop :: App ()
programLoop = do
  answer <- initSession
  count  <- runSession answer
  _      <- reportSession count
  repeatIfAsked
  where
    initSession         = readLevel >>= randomPositive
    runSession answer   = execRWST sessionLoop answer () <&> (getSum. snd)
    reportSession count = putTextLn [i|You got it in #{show count} guesses!|]
    repeatIfAsked       = askYesNo "Play again? " >>= \case
      Yes -> programLoop
      No  -> putTextLn "Thanks for playing!"

main :: IO ()
main = runProgram programLoop
