-- Function to check if a list is palindromic (that is, that it reads 
-- the same forwards and backwards.)
-- Three cases to cover: Empty, List of one element, List with more than one
-- element.
palindrome :: [a] -> Bool
palindrome [] = True
palindrome [_] = True
palindrome x = if x == reverse x then True else False
