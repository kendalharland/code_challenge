isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome list
    | null list              = True
    | length list == 1       = True
    | head list == last list = isPalindrome (init (tail list))
    | otherwise              = False

