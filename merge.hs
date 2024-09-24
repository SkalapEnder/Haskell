-- Ord is need in comprasion below
-- Ord have GT, LT, EQ
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x > y = y : ( merge(x:xs) ys )
  | otherwise = x : ( merge xs (y:ys) )

-- Return tuples (1st half of list, 2nd half of list)
halve :: [a] -> ([a],[a])
halve xs = (take half xs, drop half xs) 
    where half = length xs `div` 2

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [lst] = [lst]
-- fst - first elem of tuple, snd - second elem of yuple
mergeSort lsts = merge (mergeSort (fst halves)) (mergeSort (snd halves))
    where halves = halve lsts

main :: IO()
main = do
    -- Integer list
    putStrLn "Write your unsorted list of Integers"
    listStr <- getLine
    let mergeListInt = read listStr :: [Int]

    putStr "\nMerged and Sorted list (Integers): "
    print $ mergeSort mergeListInt

    -- Float list
    putStrLn "Write your unsorted list of Floats"
    listStr <- getLine
    let mergeListFloat = read listStr :: [Float]

    putStr "\nMerged and Sorted list (Integers): "
    print $ mergeSort mergeListFloat