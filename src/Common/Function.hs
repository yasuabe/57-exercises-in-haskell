module Common.Function (maybeIf) where

maybeIf :: Bool -> a -> Maybe a
maybeIf b a = if b then Just a else Nothing
