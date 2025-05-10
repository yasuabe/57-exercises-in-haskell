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

import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Writer.Class (tell)
import Control.Monad.RWS ( RWST, lift, execRWST )
import Control.Monad.RWS.Lazy (ask)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Monoid (Sum(..))
import Data.String.Interpolate (i)
import Data.Text (Text, unpack)
import Debug.Trace (trace)
import System.Console.Haskeline (InputT)
import System.Random (getStdGen, uniformR)
import Text.Read (readMaybe)

import Common.App (runProgram)
import Common.System (YesNo(..), askYesNo, readLine, putText, putTextLn)

type App = InputT IO

type Session = RWST Int (Sum Int) () App

randomPositive :: (MonadIO m) => Int -> m Int
randomPositive upperBound = fst . uniformR (1, upperBound) <$> getStdGen

evaluateInput :: Int -> Text -> Maybe Ordering
evaluateInput answer input = do
  readMaybe (unpack input) & \case
    Just guess -> compare guess answer & Just
    Nothing    -> trace (show input) Nothing

sessionLoop :: Session ()
sessionLoop = do
  answer <- ask
  input  <- lift $ readLine "Enter a number: "
  tell 1
  evaluateInput answer input & \case
    Just LT -> whenLT
    Just GT -> whenGT
    Just EQ -> return ()
    Nothing -> whenInvalid
  where
    proceed :: Text -> Session ()
    proceed reply = do
      putText reply & lift 
      sessionLoop
    whenLT      = proceed "Too low. Guess again: "
    whenGT      = proceed "Too High. Guess again: "
    whenInvalid = proceed "Invalid. Guess again: "

readLevel :: InputT IO Int
readLevel = tryReadLevel >>= \case
  Just n | n >= 1 && n <= 3 -> return $ 10 ^ n
  _                         -> do
    putTextLn "Invalid choice. Please try again."
    readLevel
  where
    tryReadLevel :: InputT IO (Maybe Int)
    tryReadLevel =   readLine "Pick a difficulty level (1, 2, or 3): "
                 <&> readMaybe . unpack

programLoop :: InputT IO ()
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
