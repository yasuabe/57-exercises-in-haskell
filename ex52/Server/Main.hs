{-# LANGUAGE OverloadedStrings #-}

-- Ex52: Creating Your Own Time Service
-- 
-- ・ Build a minimal web server that returns the current time as JSON: { "currentTime": "2050-01-24 15:06:26" }.
-- ・ Build a client that fetches this JSON, parses it, and displays the time in a readable format.
-- ・ Server must set Content-Type: application/json.
-- ・ Keep server code minimal.

module Main where

import Web.Scotty
import Data.Aeson (object, (.=))
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import Data.Functor ((<&>))

getTime:: ActionM ()
getTime = do
  timeStr <- currentTime
  json $ object ["currentTime" .= timeStr]
  where
    currentTime = liftIO getCurrentTime <&> formatTime defaultTimeLocale "%F %T"

main :: IO ()
main = scotty 3000 $ get "/time" getTime
