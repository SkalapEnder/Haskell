-- Maybe - constructor that 
-- gives to probable values
-- 1) Null (Nothing) 2) Value a
--
-- Just - if we get value instead null
myPeek :: [a] -> Maybe a
myPeek [] = Nothing
myPeek [xs] = Just xs -- base case
myPeek (x:xs) = myPeek xs

peek :: [a] -> Maybe a
peek [] = Nothing
peek x = Just (last x)

myIsEmpty :: [a] -> Bool
myIsEmpty [] = True -- If list is initially empty
myIsEmpty (x:_) = False 
-- _ - Wildcard Pattern operator
--Just ignores, in this case, other part of list
--If list contains 1 or more elements

--Function with built-in init
dequeue :: [a] -> [a]
dequeue = init --return list without last one

--Analogue of function above
myDequeue :: [a] -> [a]
myDequeue [x] = [] 
--If list has only 1 element
--Then it "removes" and return empty list
myDequeue (x:xs) = x : myDequeue xs

--Enqueue function with built-in operator
enqueue :: a -> [a] -> [a]
enqueue x lst = x : lst --Just join to start

--Analogue of function above
myEnqueue :: a -> [a] -> [a]
--Base case (if we had last 2 elements)
myEnqueue x [lst] = [x, lst]
myEnqueue x (lst:lsts) = x : myEnqueue lst lsts
-- : - "cons" operator