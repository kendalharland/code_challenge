module Problem15 where

repli :: [a] -> Int -> [a]
repli (x:xs) n = replicate n x ++ repli xs n
repli [] n     = []

