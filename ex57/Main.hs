{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <&>" #-}

-- Ex57: Trivia App
-- 
-- ・ Load questions, correct answers, and wrong answers from a local file.
-- ・ Randomize both:
--   ・ Question selection.
--   ・ Answer order (correct + distractors).
-- ・ Ends on first incorrect answer or all correct.
-- ・ Track number of correct answers.
-- # Constraint:
-- ・ Use a local file (not Redis or RDB) to store the question data.

module Main where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Writer
import Control.Monad.Extra (ifM)
import Data.Aeson (FromJSON, eitherDecode)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Monoid (Sum(..))
import Data.String.Interpolate (i)
import Data.Text (Text)
import GHC.Generics (Generic)
import System.Console.Haskeline
import qualified Data.ByteString.Lazy as BL

import Common.App (runProgram)
import Common.System (putTextLn, repeatUntilValid, convertText)
import Common.Util (shuffle)
import Control.Monad.IO.Class

type TriviaSession = WriterT (Sum Int) (InputT IO)

data Question = Question
  { question    :: Text
  , correct     :: Text
  , distractors :: [Text]
  } deriving (Show, Eq, Generic, FromJSON)

loadQuestions :: IO [Question]
loadQuestions =
  BL.readFile questionFilePath
    <&> eitherDecode
    >>= \case
      Right questions -> return questions
      Left  err       -> error $ "Failed to parse JSON: " ++ err

readAnswer :: TriviaSession Int
readAnswer = lift $ repeatUntilValid convertText
                                     "Your answer: "
                                     "Please enter a valid number."

askAndEvaluate :: Question -> TriviaSession Bool
askAndEvaluate (Question q c ds) = do
  (correctIndex, options) <- prepareQuestion
  putTextLn q & lift
  mapM_ printOption options
  readAnswer <&> (== correctIndex)
  where
    printOption :: (Int, Text) -> TriviaSession ()
    printOption (index, option) = lift $ putTextLn [i|#{index}. #{option}|]

    prepareQuestion = do
      shuffled <- shuffle ((c, True): map (, False) ds) & liftIO
      let indexed = zip [1..] shuffled
      return (
        find (\(_, (_, d)) -> d) indexed & (fst . fromJust),
        map (\(a, (b, _)) -> (a, b)) indexed)

questionFilePath :: FilePath
questionFilePath = "ex57/data/trivia.json"

session :: [Question] -> TriviaSession ()
session [] = return ()
session (qh:qs) =
  ifM (askAndEvaluate qh)
      (do
        tell (Sum 1)
        putTextLn "Correct!" & lift
        session qs)
      (do
        putTextLn "Wrong!" & lift
        putTextLn [i|The correct answer was #{correct qh}.|] & lift)

program :: InputT IO ()
program = do
  questions <- loadQuestions >>= shuffle & liftIO
  Sum count <- execWriterT $ session questions
  putTextLn [i|You answered #{count} answeres correctly.|]

main :: IO ()
main = runProgram program
