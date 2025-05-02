{-# LANGUAGE OverloadedStrings #-}
-- ## Ex56: Tracking Inventory
-- - Goal: Create a program to track personal inventory.
-- - Input: Item name, serial number, and estimated value.
-- - Output: A tabular report in both HTML and CSV formats.
-- - Constraints:
--   - Store data persistently in a local file using JSON, XML, or YAML.
--   - The value must be numeric.

import System.Directory (renameFile)

import Web.Scotty (get, post, html, scotty, formParam, ActionM, setHeader, text)
import Lucid (renderText)
import Data.Aeson (eitherDecode, encode)
import Control.Monad.IO.Class (liftIO)
import Data.Either (fromRight)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL

import View (Item(..), inventoryPage)

inventoryFile :: FilePath
inventoryFile = "ex56/data/inventory.json"

loadInventory :: FilePath -> IO (Either String [Item])
loadInventory path = do
  content <- BL.readFile path
  return (eitherDecode content)

loadInventoryM :: ActionM [Item]
loadInventoryM = do
  result <- liftIO $ loadInventory inventoryFile
  return $ fromRight [] result

showInventory :: ActionM ()
showInventory = do
  items <- loadInventoryM
  html (renderText (inventoryPage items))

downloadCsv :: ActionM ()
downloadCsv = do
  items <- loadInventoryM
  let csv = unlines $ map (\(Item n s v) -> n ++ "," ++ s ++ "," ++ show v) items
  setHeader "Content-Type" "text/csv"
  text (TL.pack csv)

addItem :: ActionM ()
addItem = do
  itemToAdd <- parseItemFromForm
  items     <- loadInventoryM
  let updatedInventory = items ++ [itemToAdd]
  saveInventory updatedInventory
  html (renderText (inventoryPage updatedInventory))
  where
    parseItemFromForm = Item
      <$> formParam "name"
      <*> formParam "serial_no"
      <*> formParam "value"
    saveInventory updatedItems = liftIO $ do
      let tmp = "ex56/inventory.tmp"
      BL.writeFile tmp (encode updatedItems)
      renameFile   tmp inventoryFile

main :: IO ()
main = scotty 3000 $ do
  get  "/ex56"     showInventory
  post "/ex56"     addItem
  get  "/ex56/csv" downloadCsv
