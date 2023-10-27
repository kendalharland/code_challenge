module Problem13 where

import Problem11

encodeDirect :: (Eq a) => [a] -> [Quantity a]
encodeDirect (x:xs) = encodeDirect' 1 x xs

encodeDirect' :: (Eq a) => Int -> a -> [a] -> [Quantity a]
encodeDirect' n x [] = [quantify n x]
encodeDirect' n x (y:ys)
  | x == y = encodeDirect' (n + 1) x ys
  | otherwise = (quantify n x) : encodeDirect' 1 y ys

quantify :: Int -> a -> Quantity a
quantify 1 x = Single x
quantify n x = Multiple n x

