-- # Ex44: Product Search
--
-- ・ Prompt user for a product name.
-- ・ Load product data from a JSON file.
-- ・ Search for a matching product.
-- ・ If found, display its name, price, and quantity.
-- ・ If not found, prompt again.

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad.Reader (ReaderT (..))
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans (lift)
import Data.Aeson (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy as BL
import Data.Function ((&))
import Data.List (find)
import Data.String.Interpolate (i)
import Data.Text as T (Text)
import GHC.Generics (Generic)
import System.Console.Haskeline (InputT)

import Common.App (runProgram)
import Common.System (readLine, putTextLn)
import Control.Monad.Catch (throwM)

type AppType = ReaderT [Product] (InputT IO)

productsFilePath :: FilePath
productsFilePath = "ex44/data/products.json"

newtype Products = Products
  { products :: [Product]
  } deriving (Show, Eq, Generic, FromJSON)

data Product = Product
  { name     :: Text
  , price    :: Double
  , quantity :: Int
  } deriving (Show, Eq, Generic, FromJSON)

loadProducts :: IO [Product]
loadProducts = do
  content <- BL.readFile productsFilePath
  case eitherDecode content of
    Right ps -> return $ products ps
    Left err -> throwM $ userError $ "Failed to parse JSON: " ++ err

program :: AppType ()
program = do
  inputName <- lift $ readLine "What is the product name? "
  products' <- ask
  find ((== inputName). name) products' & \case
    Just p  -> printProduct p
    Nothing -> do
      putTextLnT "Sorry, that product was not found in our inventory."
      program  -- tail recursion
  where
    printProduct :: Product -> AppType () 
    printProduct p = do
      putTextLnT [i|Name: #{name p}|]
      putTextLnT [i|Price: #{price p}|]
      putTextLnT [i|Quantity: #{quantity p}|]
    putTextLnT = lift . putTextLn

main :: IO ()
main = do
  products' <- loadProducts
  runProgram $ runReaderT program products'
