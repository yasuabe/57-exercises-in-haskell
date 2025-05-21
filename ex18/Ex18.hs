{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Ex18 where

import Data.Text as T (Text, strip, toLower)
import Data.Function ((&))
import Common.System (Converter)

data ConversionType = FtoC | CtoF deriving (Show, Read)

toConversionType :: Converter ConversionType
toConversionType input = T.strip input
                  & T.toLower
                  & \case 
                      "c" -> Just FtoC
                      "f" -> Just CtoF
                      _   -> Nothing

convert :: Fractional a => ConversionType -> a -> a
convert FtoC fahrenheit = (fahrenheit - 32) * 5 / 9
convert CtoF celsius    = (celsius * 9 / 5) + 32

from :: ConversionType -> Text
from FtoC = "Fahrenheit"
from CtoF = "Celsius"

to :: ConversionType -> Text
to FtoC = "Celsius"
to CtoF = "Fahrenheit"
