-- Create own data type with specific fields
data Book = Book {
    title       :: String,
    author      :: String,
    pages       :: Int,
    available   :: Bool
} deriving (Eq, Show, Read)

-- #########################################

-- List of available books function
-- Function 1
filterAvailableBooks :: [Book] -> [Book]
filterAvailableBooks books = filter isAvailable books

isAvailable :: Book -> Bool
isAvailable book 
    | available book == True = True
    | otherwise = False

-- Function 2 (Short Version through field)
isAvailable2 :: Book -> Bool
isAvailable2 book = available book

--Function 3 (through lambda)
filterAvailableBooks2 :: [Book] -> [Book]
filterAvailableBooks2 books = filter (\book -> available book) books

-- #########################################

-- List of books by author function
booksByAuthor :: [Book] -> String -> [Book]
booksByAuthor books authorName = filter (\book -> author book == authorName) books

-- #########################################

-- Takes list of books and book's title
-- Then search specific Title and make it unavailable
-- If we get another book, we just pass it
markAsBorrowed :: [Book] -> String -> [Book]
markAsBorrowed books bookTitle = map updateBook books
  where
    updateBook book
      | title book == bookTitle = book { available = False }
      | otherwise = book

-- Data
books :: [Book]
books = [ 
            Book "Haskell in Depth" "Vitaly Bragilevsky" 300 True, 
            Book "Learn You a Haskell" "Miran Lipovaca" 400 False,
            Book "Real World Haskell" "Bryan O'Sullivan" 500 True,
            Book "Object-Oriented Programming on C++" "Martin Fowler" 374 True,
            Book "Clean Code" "Robert Martin" 464 True,
            Book "The Pragmatic Programmer" "Andrew Hunt" 352 False,
            Book "Code Complete" "Steve McConnell" 960 True,
            Book "The Clean Coder" "Robert Martin" 256 True,
            Book "Design Patterns" "Erich Gamma" 395 False,
            Book "Refactoring" "Martin Fowler" 448 True,
            Book "The Mythical Man-Month" "Frederick P. Brooks Jr." 336 False,
            Book "Continuous Delivery" "Jez Humble" 512 True,
            Book "Head First Design Patterns" "Eric Freeman" 694 False,
            Book "Patterns of Enterprise Application Architecture" "Martin Fowler" 533 True,
            Book "Working Effectively with Legacy Code" "Michael Feathers" 456 True,
            Book "Test-Driven Development by Example" "Kent Beck" 240 False,
            Book "Cleanest Code 2nd Edition" "Robert Martin" 378 True,
            Book "Effective Java" "Joshua Bloch" 416 True
            ]

-- Count books by specific author
numsBooksByAuthor :: [Book] -> String -> Int
numsBooksByAuthor [] _ = 0 -- If empty list
numsBooksByAuthor (x:xs) authorName
-- If is the book with that author, count the book
-- Otherwise, skip it
  | author x == authorName = 1 + numsBooksByAuthor xs authorName
  | otherwise = numsBooksByAuthor xs authorName 

-- Convert Book object data to understandable table
outputBooks :: [Book] -> String
outputBooks [] = "There is no books!" -- If initially empty list

outputBooks [book] = "\n  Title: " ++ title book ++ 
                    "\nAuthor: " ++ author book ++ 
                    "\nPages: " ++ show (pages book) ++ 
                    "\nIs available: " ++ show (available book)

outputBooks (book:books) = "\n  Title: " ++ title book ++ 
                            "\nAuthor: " ++ author book ++ 
                            "\nPages: " ++ show (pages book) ++ 
                            "\nIs available: " ++ show (available book) ++ "\n" ++
                            outputBooks books -- Recusrively Step

main :: IO()
main = do
  putStr "Write name of author to get books: "
  authorName <- getLine

  -- 1) Number of books by author 
  -- 2) Full list of author's books
  let numsOfBooks = numsBooksByAuthor books authorName
  let authorBooks = outputBooks (booksByAuthor books authorName)

  -- If number of books is 0 (EQ, equal), then print that there is no books
  -- Otherwise (GT, greater than), print number of books and full list 
  case compare numsOfBooks 0 of
    EQ -> putStrLn ("\n\n\tThere is no books by " ++ authorName)
    GT -> putStrLn ("\n\n\t" ++ show(numsOfBooks) ++ " book" ++ (if numsOfBooks > 1 then "s" else "") ++ " by " ++ authorName ++ ":" ++ authorBooks)

  -- Show list of available books after the update (before)
  putStr "\n\n\n\tList of available books (before):\n"
  putStrLn . outputBooks $ filterAvailableBooks2 books

  -- Mark book as borrowed
  putStr "\n\nWrite title of the book to borrow: "
  bookTitle <- getLine
  let updatedBooks = markAsBorrowed books bookTitle

  -- Show list of available books after the update (after)
  putStr "\n\n\n\tList of available books (after):\n"
  putStrLn . outputBooks $ filterAvailableBooks2 updatedBooks
  