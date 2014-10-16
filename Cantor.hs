-- A program to calculate the n-th term in the right-first Cantor's diagonalization of the rational numbers.
-- Uses tail recursion.
-- Start by passing in getterm n, which passes in the first term 1/1,
-- and direction going upwards (up as well as right).
-- flipDirection = 1 -> going up and right diagonally
-- flipDirection = 0 -> going down and left diagonally

getterm n = descend 1 1 1 1 n

descend numerator denominator flipDirection currTerm maxTerm = 
	if currTerm == maxTerm 
		then displayTerm maxTerm numerator denominator
		else if numerator == 1 && flipDirection == 1
			then descend numerator (denominator+1) 0 (currTerm+1) maxTerm
			else if denominator /= 2
				then descend (numerator+1) (denominator-1) 0 (currTerm+1) maxTerm
				else ascend (numerator+1) (denominator-1) 1 (currTerm+1) maxTerm

ascend numerator denominator flipDirection currTerm maxTerm =
	if currTerm == maxTerm
		then displayTerm maxTerm numerator denominator
		else if denominator == 1 && flipDirection == 1
			then ascend (numerator+1) denominator 0 (currTerm+1) maxTerm
			else if numerator == 2
				then descend (numerator-1) (denominator+1) 1 (currTerm+1) maxTerm
				else ascend (numerator-1) (denominator+1) 0 (currTerm+1) maxTerm

displayTerm maxTerm numerator denominator =
	putStrLn ("Term " ++ show maxTerm ++ " is " ++ show numerator ++ "/" ++ show denominator)