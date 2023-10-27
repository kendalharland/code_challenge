module Problem12 where

import Problem11

decodeModified :: [Quantity a] -> [a]
decodeModified [] = []
decodeModified (x:xs) = decodeModified' x ++ decodeModified xs

decodeModified' :: Quantity a -> [a]
decodeModified' (Single x) = [x]
decodeModified' (Multiple count x) = [x | _ <- [1..count]]

{- h99 solution -}
dm :: [Quantity a] -> [a]
dm = concatMap dmHelper
   where
       dmHelper (Single x) = [x]
       dmHelper (Multiple n x) = replicate n x 

