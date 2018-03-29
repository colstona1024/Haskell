import System.IO
import System.Random
import Data.List

randomlist:: Int -> [Int]
randomlist x = do
    let randList = [[2,6,9,10,4,8],[1,5,9,3,6,8,45,1,0],[0,4,7,33,1,5,7,8,34],[90,2,3,63,2,32,99,2,5],[18,42,49,6,19,0,13],[19,14,16,18,12,15],[200,32,9,12,5,16,7,49],[19,0,3,8,0,41,80,12,7,0,1,42],[12,3,80,9,3,1,2,64,1,42], [9,23,4,2,1,4,0,32]]
    randList !! x

randomFlist :: Int -> [Float]
randomFlist x = do
    let randList = [[2.3,3.5,4.9,1.0],[5.4,7.8,0.9,5.4,3.7,0.1,0.8,5.1],[3.0,9.1,2.9,1.3,2.0,0.9,3.2,1.0],[1.2,8.5,9.7,2.5,3.7,0.9,8.4,3.2,1.9,7.0,8.1],[4.2,7.0,9.1,3.8,7.0,9.4,2.3,1.4,9.3,0],[1.2,0.9,7.4,2.1,8.9,7.4,1.9,2.4,0.7,0.8],[8.9,4.0,3.2,1.8,0.9,3.4,2.1],[1.2,0.9,3.4,9.0,7.5,2.9,1.0,4.7],[2.1,0.9,4.8,2.8,4.0,9.8,2.4,1.0,9.4,2.1], [4.2,3.4,5.1,3.4,7.9,0.2]]
    randList !! x

mean :: [Int] -> Int
mean x = do
    sum x `div` length x

meanF :: [Float] -> Float
meanF x = do
    let division = sum x /fromIntegral(length x)
    division

median :: [Int] -> Int
median x = do
    let sorted = sort x
    let sortedLength = length sorted
    if sortedLength `mod` 2 == 0
    then
     sorted !! ((sortedLength - 1) `div` 2 )
    else
     sorted !! ((sortedLength - 1) `div` 2 + 1)
     
medianF :: [Float] -> Float
medianF x = do
    let sorted = sort x
    let sortedLength = length sorted
    if sortedLength `mod` 2 == 0
    then
     sorted !! ((sortedLength - 1) `div` 2)
    else
     sorted !! (((sortedLength - 1) `div` 2) + 1)

maxValue :: [Int] -> Int
maxValue x = do
    if length x == 1
    then x !! 0
    else
        if x !! 0  > x !! 1
        then
         maxValue (x!! 0 : (tail(tail x)))
        else
         maxValue (x!! 1 : (tail(tail x)))

maxValueF :: [Float] -> Float
maxValueF x = do
    if length x == 1
    then x !! 0
    else
        if x !! 0  > x !! 1
        then
         maxValueF (x!! 0 : (tail(tail x)))
        else
         maxValueF (x!! 1 : (tail(tail x)))

minValue :: [Int] -> Int
minValue x = do
    if length x == 1
    then x !! 0
    else
        if x !! 0  > x !! 1
        then
         minValue (x!! 1 : (tail(tail x)))
        else
         minValue (x!! 0 : (tail(tail x)))

minValueF :: [Float] -> Float
minValueF x = do
    if length x == 1
    then x !! 0
    else
        if x !! 0  > x !! 1
        then
         minValueF (x!! 1 : (tail(tail x)))
        else
         minValueF (x!! 0 : (tail(tail x)))

stdDev   :: [Int] -> Float
stdDev x = do
    -- let tempMean = mean x
    let intermed = map (subtract (mean x)) x
    let squaredValue = map (^2) intermed
    let mean2 = mean squaredValue
    -- mean2
    let squareroot = sqrt (fromIntegral mean2)
    squareroot
    -- 72.7

stdDevF :: [Float] -> Float
stdDevF x = do
    let populationMean = meanF x
    -- tempMean
    let mapPopMean = map (subtract ( populationMean)) x
    -- intermed
    let mapSquaredValue = map (^2) mapPopMean
    -- squaredValue
    let spreadMean = meanF mapSquaredValue
    -- mean2
    let squareroot = sqrt spreadMean
    squareroot

subMean :: Int -> Int -> Int
subMean x y = 
    x - y

main :: IO()
main = do
    num <- randomIO :: IO Int
    let randomNumber = num `mod` 10
    let ranList = randomlist randomNumber

    print $ "Int values"
    print $ ranList
    print $ "mean:" 
    print $ mean ranList
    print $ "median:"
    print $ median ranList
    print $ "Max:"
    print $ maxValue ranList
    print $"min:"
    print $ minValue ranList
    print $ "stdDev:"
    print $ stdDev ranList

    print $ "Float values"

    let ranFloatList = randomFlist randomNumber
    print $ ranFloatList
    print $ "mean:" 
    print $ meanF ranFloatList
    print $ "median:"
    print $ medianF ranFloatList
    print $ "Max:"
    print $ maxValueF ranFloatList
    print $"min:"
    print $ minValueF ranFloatList
    print $ "stdDev:"
    print $ stdDevF ranFloatList
