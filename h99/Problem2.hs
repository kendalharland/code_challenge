myButLast :: [a] -> Maybe a
myButLast l 
  | length l < 2  = Nothing
  | length l == 2 = Just (head l) 
  | otherwise     = myButLast (tail l)
  ;
