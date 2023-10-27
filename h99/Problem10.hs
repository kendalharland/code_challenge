module Problem10 where

encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode (x:xs) = let (matches, rest) = span (==x) xs
                in ((length matches) + 1, x) : encode rest


