--Just created function bcs
--I got used to write on C++, Java like 
readListToInt :: String -> [Int]
readListToInt lst = read lst

squareList :: [Int] -> [Int]
-- "map squareNum" means apply 
-- function 'squareNum' each element of list
squareList lst = map squareNum lst

squareNum :: Int -> Int
squareNum x = x * x

fibonacci :: Int -> Int
--Base cases
fibonacci 1 = 1
fibonacci 2 = 1
--Recursive case (number greater than 2)
fibonacci n | n > 2 = fibonacci(n-1) + fibonacci(n-2)

isEven :: Int -> Bool
isEven x = x `mod` 2 == 0

sumList:: [Int] -> Int
-- sum - built-in function
sumList lst = sum lst

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)


main :: IO()
main = do
    putStrLn "\tAssignment 1"

    --Square list finction
    putStrLn "\n\tSquare List "
    putStrLn "Enter list (e.g. [1,2,3]): "
    listStr <- getLine
    let testList = readListToInt listStr

    putStr "Square list: "
    print $ squareList testList

    --Fibonacci function
    putStrLn "\n\tFibonaci "
    putStr "Enter n-th value of Fibonacci (start from 1): "
    fibStr <- getLine
    let fib = read fibStr :: Int

    putStr (fibStr ++ "th value of Fibonacci: ")
    print $ fibonacci fib

    --Even function
    putStrLn "\n\tEven "
    putStr "Enter x to check Even: "
    xStr <- getLine
    let x = read xStr :: Int

    putStr "Is Even: "
    print $ isEven x

    --Sum List function
    putStrLn "\n\tSum of List's elements "
    putStr "Enter list: "
    listStr <- getLine
    let testList = readListToInt listStr

    putStr "Sum of list: "
    print $ sumList testList

    --Swap function
    putStrLn "\n\tSwap "
    putStr "Enter a: "
    aStr <- getLine
    let a = read aStr :: Int
    putStr "Enter b: "
    bStr <- getLine
    let b = read bStr :: Int


    putStrLn "Swaped: "
    -- let (a, b) = swap (a, b)
    -- This statement output
    -- infinite loop exception
    -- I still dunno how it happens
    let (newA, newB) = swap (a, b)
    putStr "A: "
    print newA
    putStr "\nB: "
    print newB