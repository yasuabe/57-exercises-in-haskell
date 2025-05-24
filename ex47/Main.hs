-- Ex47: Who’s in Space?
--
-- ・Access live data from the Open Notify API (http://api.open-notify.org/astros.json).
-- ・Parse the JSON response.
-- ・Display:
--   ・Total number of people in space.
--   ・A table of names and spacecraft.
-- ・Do not use pre-downloaded data.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

import Data.Aeson (FromJSON)
import Data.String.Interpolate (i)
import Network.HTTP.Simple
import Data.Text as T (Text, length, replicate, justifyLeft)
import GHC.Generics (Generic)
import System.Console.Haskeline (InputT)
import Control.Monad.IO.Class (liftIO)
import Data.List.NonEmpty (NonEmpty(..))

import Common.App (runProgram)
import Common.System (putTextLn)
import Common.Tuples (MaxPair(..), Pair(..))

data Astros = Astros
    { people  :: [Person]
    , number  :: Int
    } deriving (Show, Generic, FromJSON)

data Person = Person
    { name  :: Text
    , craft :: Text
    } deriving (Show, Generic, FromJSON)

display :: Astros -> InputT IO ()
display Astros {people = ps, number = num} = do
  putTextLn [i|There are #{num} people in space right now:|]
  mapM_ printRow $ headerRow :| separator : recordRows

  where
    toWidthPair    = (\(Pair p) -> MaxPair p) . fmap T.length . Pair

    toRow p         = (" " <> name p ," " <> craft p)
    headerRow       = (" Name", " Craft")
    recordRows      = map toRow ps
    MaxPair(w1, w2) = foldMap toWidthPair (headerRow :| recordRows)

    printRow (c1, c2) = putTextLn [i|#{pad w1 c1}|#{pad w2 c2}|]

    Pair separator = fmap hyphens $ Pair (w1, w2)
      where hyphens n = T.replicate (n + 1) "-"
    pad width = T.justifyLeft width ' '

program :: InputT IO ()
program =
  liftIO (httpJSON "http://api.open-notify.org/astros.json")
  >>= (display . getResponseBody)

main :: IO ()
main = runProgram program
