module Problem17 where

split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split elems n
  | n >= length elems = (elems, [])
  | otherwise         = (take n elems, drop n elems)