-- Function to find the number of elements in a list.
-- Usage: findNumElements [someList] 1
findNumElements [h] x = x
findNumElements (h:t) x = findNumElements t (x+1)
