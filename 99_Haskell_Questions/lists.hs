-- Take the first element from a list.
-- Option 1:
getFirstElementFromList :: [a] -> [a]
getFirstElementFromList l = take 1 l

-- Option 2 (Wrote this after taking class on programming languages which taught some formal Haskell):
getFirstElementFromList2 :: [a] -> a 
getFirstElementFromList2 (x:xs) = x


