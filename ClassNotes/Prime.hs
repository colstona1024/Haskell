ip _ [] = True
ip n (x:xs)
    | n `mod` x == 0 = False
    | otherwise = ip n xs

isPrime n = ip n [ 2 .. (n `div` 2)]
