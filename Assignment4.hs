import Data.Char
import Data.List

let2nat :: Char -> Int
let2nat x =
    ord x - 97


nat2let :: Int -> Char
nat2let x = 
    chr (x + 97)


shift :: Int -> Char -> Char
shift x y =
    if isLower y && isLetter y
    then
        nat2let (((let2nat y) + x) `mod` 26)
    else y


encode :: Int -> String -> String
encode x y =
    map (shift x) (y)


decode :: Int -> String -> String
decode x y = 
    map (shift (-x)) (y)


table :: [Float]
table = [8.2,1.5,2.8,4.3,12.7,2.2,2.0,6.1,7.0,0.2,0.8,4.0,2.4,6.7,7.5,1.9,0.1,6.0,6.3,9.1,2.8,1.0,2.4,0.2,2.0,0.1]


lowers :: [Char] -> Int
lowers x =
    if length x == 1
    then
        if isLower (head x)
        then
            1
        else 0
    else
        if isLower (head x)
        then
            1 + lowers (tail x)
        else
            0 + lowers (tail x)


count :: Char -> String -> Int
count x y =
    if length y == 1
    then
        if head y == x
        then
            1
        else
            0
    else
        if head y == x
        then
            1 + count x (tail y)
        else
            0 + count x (tail y)


percent :: Int -> Int -> Float
percent x y =
    ((fromIntegral x) / (fromIntegral y)) * 100


freqs :: String -> [Float]
freqs x =
    map (`percent` (lowers x)) (map (`count` x) ['a'..'z'])


rotate :: Int -> [a] -> [a]
rotate x y =
    if x < 1
    then
        y
    else
        rotate (x-1) ((tail y)++[(head y)])


chisqr :: [Float] -> [Float] -> Float 
chisqr x y =
    if length x <2
    then
        (((head x)-(head y))**2)/(head y)
    else
        ((((head x)-(head y))**2)/(head y)) + (chisqr (tail x) (tail y))


position :: Eq a => a -> [a] -> Int
position x y =
    if (head y) == x
        then
        0
    else
        1 + position x (tail y)


crack :: String -> String
crack x = do
    let temp2 = map (`chisqr` table) (map (`rotate` (freqs x)) [0..25])
    let index = minimum temp2
    decode (position (minimum temp2) temp2) x


main :: IO()
main = do
    print $ crack (encode 3 "Allen_is_the_very_best")