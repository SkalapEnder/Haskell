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


books :: [Book]
books = [ 
            Book "Haskell in Depth" "Vitaly Bragilevsky" 300 True, 
            Book "Learn You a Haskell" "Miran Lipovaca" 400 False,
            Book "Real World Haskell" "Bryan O'Sullivan" 500 True,
            Book "Clean Code" "Robert C. Martin" 464 True,
            Book "The Pragmatic Programmer" "Andrew Hunt" 352 False,
            Book "Code Complete" "Steve McConnell" 960 True,
            Book "The Clean Coder" "Robert C. Martin" 256 True,
            Book "Design Patterns" "Erich Gamma" 395 False,
            Book "Refactoring" "Martin Fowler" 448 True,
            Book "The Mythical Man-Month" "Frederick P. Brooks Jr." 336 False,
            Book "Continuous Delivery" "Jez Humble" 512 True,
            Book "Head First Design Patterns" "Eric Freeman" 694 False,
            Book "Patterns of Enterprise Application Architecture" "Martin Fowler" 533 True,
            Book "Working Effectively with Legacy Code" "Michael Feathers" 456 True,
            Book "Test-Driven Development by Example" "Kent Beck" 240 False,
            Book "Effective Java" "Joshua Bloch" 416 True
            ]

-- Convert Book object data to understandable table
outputBooks :: [Book] -> String
outputBooks [] = "There is no books!"
outputBooks [book] = "\n\nTitle: " ++ title book ++ 
                    "\nAuthor: " ++ author book ++ 
                    "\nPages: " ++ show (pages book) ++ 
                    "\nIs available: " ++ show (available book)
outputBooks (book:books) = "\n\nTitle: " ++ title book ++ 
                            "\nAuthor: " ++ author book ++ 
                            "\nPages: " ++ show (pages book) ++ 
                            "\nIs available: " ++ show (available book) ++ 
                            outputBooks books

main :: IO()
main = do
    putStrLn "List of books by Robert Martin:"
    putStrLn . outputBooks $ filterAvailableBooks2 books

