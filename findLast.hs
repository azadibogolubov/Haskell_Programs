-- Recursive function to find the last element of a list.
findLast :: [a] -> a
findLast [h] = h
findLast (h:t) = findLast t
