module Problem16 where

dropEvery :: [a] -> Int -> [a]
dropEvery (x:xs) n = drop' (x:xs) n n
dropEvery [] _ = []

drop' :: [a] -> Int -> Int -> [a]
drop' (x:xs) n k
  | k > 1  = x : drop' xs n (k-1)
  | k == 1 = drop' xs n n
drop' [] _ _ = []
  