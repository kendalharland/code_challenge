myReverse :: [a] -> [a]
myReverse list 
    | null list = []
    | otherwise = (last list) : (myReverse $ init list)
