module Ex29 (ruleOf72) where

import Control.Arrow ((>>>))

ruleOf72 :: Int -> Int
ruleOf72 = fromIntegral >>> ((72.0 :: Double) /) >>> round
