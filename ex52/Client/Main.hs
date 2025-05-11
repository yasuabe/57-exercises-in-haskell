{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Aeson ( FromJSON, decode, FromJSON(..), withObject, (.:) )
import Data.Function ((&))
import Data.String.Interpolate (i)
import Data.Time (
  UTCTime, formatTime, defaultTimeLocale, parseTimeM )
import Network.HTTP.Simple (httpLBS, getResponseBody, parseRequest)

newtype TimeResponse = TimeResponse { currentTime :: UTCTime } deriving Show

instance FromJSON TimeResponse where
  parseJSON = withObject "TimeResponse" $ \v -> do
    tStr <- v .: "currentTime"
    t    <- parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" tStr
    return (TimeResponse t)

printTime :: UTCTime -> IO ()
printTime ut = do
  let str = formatTime defaultTimeLocale "%T UTC %B %-e %Y." ut
  putStrLn [i|The current time is #{str}|]

main :: IO ()
main = do
  request  <- parseRequest "http://localhost:3000/time"
  response <- httpLBS request
  getResponseBody response & decode & \case
    Just (TimeResponse t) -> printTime t
    Nothing               -> putStrLn "Failed to parse JSON response"
