ip _ [] = True
ip n (x:xs)
    | n `mod` x == 0 = False
    | otherwise = ip n xs

isPrime n = ip n [ 2 .. (n `div` 2)]

largestPrimeFactor :: Int -> Int
largestPrimeFactor n =
	if n == 0 
		then
    head [x | x <- [1..n], isPrime x == True]
    else n

smallestPrimeFactor :: Int -> Int
smallestPrimeFactor n =
    head [x | x <- [2..n], n `mod` x == 0]