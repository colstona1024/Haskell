import Data.List

insertionSort :: [Int] -> [Int]
insertionSort x = do
    if length x > 2
    then
        insert (head x) (insertionSort (tail x))
    else 
        insert (head x) (tail x)

bubbleSort :: [Int] -> [Int]
bubbleSort (x) = do
    let pass = bubbleSortHelper x
    if checkList pass
    then
        pass
    else 
        bubbleSort pass


bubbleSortHelper :: [Int] -> [Int]
bubbleSortHelper [] = do
    []
bubbleSortHelper [x] = do
    [x]
bubbleSortHelper (x:y:[]) = do
    if x < y
    then
        x:[y]
    else
        y:[x]
bubbleSortHelper (x:y:xs) = do
    if x < y
    then
        x : (bubbleSortHelper (y:xs))
    else
        y : (bubbleSortHelper (x:xs))

checkList :: (Ord a) => [a] -> Bool
checkList [] = True
checkList [x] = True
checkList (x:y:xs) = x <= y && checkList (y:xs)

selectionSort :: [Int] -> [Int]
selectionSort x = do
    if length x < 2
    then
        x
    else
        (minimum x) : selectionSort (delete (minimum x) x)


main :: IO()
main = do
    let unsorted = [5,22,0,3,19,30,42,8,2,5,3,31,7]
    print $ "List to sort:"
    print $ unsorted
    print $ "Insertion Sort :"
    print $ insertionSort unsorted
    print $ "Insertion Sort :"
    print $ bubbleSort unsorted
    print $ "Insertion Sort :"
    print $ selectionSort unsorted