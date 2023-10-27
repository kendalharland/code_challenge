{-
  Problem: 
    Determine whether it's possible to remove, at most, a single element from 
    a list L of size N to create a strictly-increasing sequence.
    
  Constraints:
    * N <= 2
    
  Solution:
    It's possible to create a strictly-increasing sequence by removing at most
    a single element from L iff L is already strictly-increasing or there exists
    a single element x_i in L such that:
      * x_i >= x_(i+1)
      * A strictly-increasing sequence can be created by removing either x_i
        or x_(i+1) from L.
    
   Algorithm: 
     1. if N == 2, return true.
     2. Find the first occurrence of x_i in L.
     3. We know that [x_0 .. x_i] is a strictly-increasing sequence.
     4. If either of the sequences [x_0 .. x_i, x_(i+2) .. x_N] or 
        [x_0 .. x_(i-1), x_(i+1) .. x_N] is strictly-increasing, return true. Else
        return false.
-}
almostIncreasingSequence :: [Int] -> Bool
almostIncreasingSequence (x:xs) = findDroppableItem Nothing x (head xs) (tail xs)

findDroppableItem :: (Maybe Int) -> Int -> Int -> [Int] -> Bool
findDroppableItem Nothing x next [] = True
findDroppableItem Nothing x next rest
  | x >= next = (x < head rest || next < head rest) && isIncreasing rest
  | otherwise = findDroppableItem (Just x) next (head rest) (tail rest)
  ;
 
findDroppableItem (Just prev) x next [] = True
findDroppableItem (Just prev) x next rest
  | x >= next = (x < head rest || (next > prev && next < head rest)) && isIncreasing rest
  | otherwise = findDroppableItem (Just x) next (head rest) (tail rest)
  ;
  
isIncreasing [] = True
isIncreasing (x:xs)
  | null xs = True
  | x < (head xs) = isIncreasing xs
  | otherwise = False
  ;
