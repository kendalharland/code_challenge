elementAt :: [a] -> Int -> Maybe a
elementAt xs i
    | null xs  = Nothing
    | otherwise = Just (xs !! i)