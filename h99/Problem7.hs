data NestedList a = Elem a | List [NestedList a]

myFlatten :: NestedList a -> [a]
myFlatten (List [])     = []
myFlatten (Elem x)      = [x]
myFlatten (List (x:xs)) = myFlatten x ++ myFlatten (List xs) 
