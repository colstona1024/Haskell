-- Run problem 1, problem 2, problem 3 with each parameter to answer each test question 

-- Start Problem 1
problem1 :: Int -> [Int]
problem1 n = 
    [x | x <- [1..n], x `mod` 7 == 0, x `mod` 3 == 0, x `mod` 2 /=0]

-- start problem 2
problem2 :: Int -> Int
problem2 n = do
    -- divisors of each n
    let divisors = map (problem2Helper) [1..n]
    let temp2 = filter (problem2Helper2) divisors
    length temp2

problem2Helper :: Int -> [Int]
problem2Helper n = 
    [x | x <- [1..(n)], n `rem` x == 0]

problem2Helper2:: [Int] -> Bool
problem2Helper2 x = 
    if length x == 8
    then
        True
    else
        False

-- start problem 3
problem3 :: [[Int]]
problem3 =
    [palindrone 3] ++ [palindrone 4] ++ [palindrone 5]

palindrone :: Int -> [Int]
palindrone  n = 
    [x | x <- [10..(10^(n-1))], x `mod` (10^(n-2)) == x `div` (10^(n-2)), x `mod` n == 0]

