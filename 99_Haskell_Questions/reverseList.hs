-- Function to reverse a list.
-- There are two cases: The empty list, and the non-empty list.
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (h:t) = reverse t ++ [h]
