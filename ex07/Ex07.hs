module Ex07 where

conversionFactor :: Double
conversionFactor = 0.09290304

toSquareMeter :: Double -> Double
toSquareMeter squareFeet = squareFeet * conversionFactor
