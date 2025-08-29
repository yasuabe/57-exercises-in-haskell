module Common.Function (fproduct, maybeIf) where

maybeIf :: Bool -> a -> Maybe a
maybeIf b a = if b then Just a else Nothing

fproduct :: Functor f => (a -> b) -> f a -> f (a, b)
fproduct f = fmap (\a -> (a, f a))
