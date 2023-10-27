pack :: (Eq a) => [a] -> [[a]]
pack list = pack' [] list

pack' :: (Eq a) => [a] -> [a] -> [[a]]
pack' _ [] = []
pack' buf (x:xs)
    | null buf && null xs = [[x]]
    | null buf            = pack' [x] xs
    | null xs             = [buf, [x]]
    | x == head buf       = pack' (x : buf) xs  
    | otherwise           = buf : pack' [x] xs  

pack2 :: (Eq a) => [a] -> [[a]]
pack2 (x:xs) = let (matches,rest) = span (==x) xs
		in (x:matches) : pack2 rest 
pack2 [] = []
