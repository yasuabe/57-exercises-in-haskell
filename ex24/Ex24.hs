module Ex24 where

import Data.Text as T (Text, unpack)
import Data.List ( sort )

isAnagram :: Text -> Text -> Bool
isAnagram str1 str2 = 
  let sortedStr1 = sort (unpack str1)
      sortedStr2 = sort (unpack str2)
  in sortedStr1 == sortedStr2