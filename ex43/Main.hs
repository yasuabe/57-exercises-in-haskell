{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- # Ex43: Website Generator
--
-- - Prompt for:
--   - Site name
--   - Author name
--   - Whether to create js/ and css/ folders
-- - Generate:
--   - A site root folder named after the site
--   - index.html with <title> and <meta> using input
--   - Optionally: js/ and/or css/ subfolders
-- - Output messages confirming each created file/folder.

module Main where

import Common.App (runProgram)
import Common.System (YesNo (..), askYesNo, putTextLn, readLine)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.String.Interpolate (i, __i)
import Data.Text
import System.Console.Haskeline (InputT)
import System.Directory (createDirectoryIfMissing)

type App = InputT IO

htmlContent :: Text -> Text -> Text
htmlContent author siteName = [__i|
  <!DOCTYPE html>
  <html lang="en">
    <head>
        <meta charset="UTF-8">
        <meta name="author" content="#{author}">
        <title>#{siteName}</title>
    </head>
    <body>
    </body>
  </html>
  |]

createDirectory :: Text -> Text -> InputT IO ()
createDirectory parent folderName = do
  putTextLn [i|Created ./#{path}|]
  liftIO $ createDirectoryIfMissing True $ unpack path
  where
    path = parent <> "/" <> folderName

createIndexHtml :: Text -> Text -> InputT IO ()
createIndexHtml siteName author = do
  let content  = htmlContent author siteName
  let filePath = siteName <> "/index.html"

  liftIO $ writeFile (unpack filePath) (unpack content)
  putTextLn [i|Created ./#{filePath}|]

programLoop :: App ()
programLoop = do
  siteName  <- readLine "Site name: "
  author    <- readLine "Author: "
  jsFolder  <- askYesNo "Do you want a folder for JavaScript? (y/n): "
  cssFolder <- askYesNo "Do you want a folder for CSS? (y/n): "

  let mkdir = createDirectory siteName

  mkdir ""
  createIndexHtml siteName author

  when (jsFolder == Yes) $ mkdir "js/"
  when (cssFolder == Yes) $ mkdir "css/"

main :: IO ()
main = runProgram programLoop