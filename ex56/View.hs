{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module View (inventoryPage, Item(..)) where

import GHC.Generics (Generic)
import Data.Aeson
    ( FromJSON(..), ToJSON(..), fieldLabelModifier, defaultOptions, ToJSON, FromJSON )  -- must import parseJSON explicitly
import Lucid
import Data.Text ( Text )
import Data.Aeson.Types (genericParseJSON, genericToJSON)

data Item = Item
  { name     :: String
  , serialNo :: String
  , value    :: Double
  } deriving (Show, Generic)

instance FromJSON Item where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = \case
        "serialNo" -> "serial_no"
        other      -> other
    }
instance ToJSON Item where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = \case
        "serialNo" -> "serial_no"
        other      -> other
    }
inventoryPage :: [Item] -> Html ()
inventoryPage items = doctypehtml_ $ do
  html_ [lang_ "en"] $ do
    head_ $ do
      meta_ [charset_ "UTF-8"]
      title_ "Ex 56: Tracking Inventory"
      style_ css
    body_ $ do
      h1_ "Ex 56: Tracking Inventory"
      section_ $ form_ [method_ "post", action_ "/ex56"] $ do
        table_ $ do
          thead_ $
            tr_ $ do
              th_ "Name"
              th_ "Serial Number"
              th_ "Value"
          tbody_ $ do
            mapM_ renderItem items
            tr_ $ do
              td_ $ input_ [type_ "text", name_ "name", required_ ""]
              td_ $ input_ [type_ "text", name_ "serial_no", required_ ""]
              td_ $ input_ [type_ "number", name_ "value", step_ "0.01", required_ ""]
        button_ [type_ "submit"] "Add"
      footer_ $
        a_ [href_ "/ex56/csv", target_ "_blank"] "CSV"

renderItem :: Item -> Html ()
renderItem (Item n s v) = tr_ $ do
  td_ (toHtml n)
  td_ (toHtml s)
  td_ (toHtml (show v))

css :: Text
css = "section { width: 500px; }\
     \ table { border: 1px #555 solid; border-collapse: collapse; width: 100%; }\
     \ table * { border: 1px #555 solid; box-sizing: border-box; }\
     \ th, td { padding: 5px; text-align: left; }"
