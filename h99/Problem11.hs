module Problem11 where

import Problem10

data Quantity a = Multiple Int a 
                | Single a
                deriving (Show)

squish :: (Eq a) => (Int, a) -> Quantity a
squish x
  | fst x == 1 = Single (snd x)
  | otherwise  = Multiple (fst x) (snd x)

encodeModified :: (Eq a) => [a] -> [Quantity a]
encodeModified list = map squish (encode list)
