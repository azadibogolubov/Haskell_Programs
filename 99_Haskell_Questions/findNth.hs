-- Recursive program to find nth element of zero based index list.
findNth :: [a] -> Integer -> a
findNth (h:t) 0 = h
findNth (h:t) x = findNth t (x-1)
