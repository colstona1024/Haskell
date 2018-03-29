-- Order n complexity
simple = [x | x <- [1..10]]


multiplesOf17 n = [x | x <- [1..n], x `mod` 17 == 0]

isqrt :: Integral i => i -> i
isqrt = floor . sqrt . fromIntegral

ip _ [] = True
ip n (x:xs)
    | n `mod` x == 0 = False
    | otherwise = ip n xs

isPrime n = ip n [2..(isqrt n)]