palindrone :: Int -> Int
palindrone n = do
    zipWith (+) [1..n] [1..n]
    32




-- palindrone :: [Int] -> Bool
-- palindrone n = do
--     if length n > 3
--         then
--             if head n == last n
--                     then
--                         True && palindrone (init (tail n))
--                     else
--                         False
--         else
--             if length n == 3
--                 then
--                 if head n == last n
--                     then
--                         True
--                     else
--                         False
--                 else
--                     if n !! 0 == n !! 1
--                     then
--                         True
--                     else
--                         False
