import Text.Read (readMaybe) 
-- Import to get Maybe type from read
-- See reason in line 127

-- Implement deriving tool
-- to get instance Show
data Item = Item{
    name :: String,
    quantity :: Int,
    price :: Float
} deriving(Show)

data Result a = Success a | Error String deriving(Show)

-- Functor instance
-- 1) If we get some f (function) and Result is Success, 
--    then implement f to inner value of Success (box)
-- 2) Otherwise, return String error message
instance Functor Result where
    fmap f (Success a) = Success (f a)
    fmap _ (Error msg) = Error msg
    -- For Error type, it doesn't matter what function will take
    -- It anyway return string message

-- Cart itself
cart :: [Result Item]
cart = [
    Success (Item "MountDew" 1 500),
    Success (Item "Doritos" 2 700),
    Error "Out of stock",
    Success (Item "Pineapple" 1 800),
    Error "Not in market"
    ]

-- ########## Calculations ##############
-- Function to add discount to each price
applyDiscountToItem :: Float -> Item -> Item
applyDiscountToItem discount item =
    item { price = price item * (1 - discount / 100) }

-- Using fmap with Result to apply the discount
-- There doesn't matter: map or fmap **
applyDiscount :: Float -> [Result Item] -> [Result Item]
applyDiscount discount = fmap (fmap (applyDiscountToItem discount))

-- I used 2 fmap for 2 reasons
-- 1) I mentioned upper **
-- 2) Functor for Result type

-- There won't be error bcs according instance of Functor
-- applyDiscountToItem will implemented only to Success Item
-- Otherwise, it will return same Error msg

-- Function to output total cost of items
-- I set Float because it anyway return either 0 or some number
calculateTotal :: [Result Item] -> Float
calculateTotal [] = 0.0
calculateTotal items = sum $ fmap takePrice items
    where
        -- Define local func with type recognizing
        -- If Success -> return price
        -- If Error -> return 0.0 (nothing)
        takePrice :: Result Item -> Float
        takePrice (Success item) = price item
        takePrice (Error msg) = 0.0
-- ########## Calculations ##############



-- ########## Outputs ##############

-- Function to output Cart with Items
outputCart :: [Result Item] -> IO()
-- Implement mapM_ bcs I don't put result anywhere
-- Also compiler complain about fmap (look to presentation)
outputCart items = mapM_ (\result -> 
    case result of
        Success item -> putStrLn ("\nName: " ++ name item ++ "\nQuantity: " ++ show (quantity item) ++ "\nPrice: " ++ show (price item))
        Error _      -> return ()  -- Ignore errors
    ) items

-- Function to output Errors
outputError :: [Result Item] -> IO()
outputError items = mapM_ (\result -> 
    case result of
        Success _ -> return () -- Ignore Success Items
        Error msg    -> putStrLn ("Error Item: " ++ msg)  
    ) items

-- ########## Outputs ##############


-- ########## My Funcs #############
-- Function to display menu options
displayMenu :: IO ()
displayMenu = do
    putStrLn "\n\t==== Shopping Cart Menu ===="
    putStrLn "1. Show cart items with total price"
    putStrLn "2. Show Error items"
    putStrLn "3. Apply discount and calculate total price"
    putStrLn "0. Exit"
    putStr "\nPlease choose an option: "

-- Main menu function with loop
menuLoop :: [Result Item] -> IO ()
menuLoop items = do
    displayMenu
    option <- getLine
    case option of
        -- Output cart
        "1" -> do
            putStrLn "\n\tCurrent cart items:"
            outputCart items
            putStrLn $ "\nTotal: " ++ show (calculateTotal items)
            menuLoop items

        -- Output errors
        "2" -> do
            putStrLn "\n\tError cart items:"
            outputError items
            menuLoop items

        -- Implement discount to cart
        "3" -> do
            putStr "\nEnter discount percentage: "
            discountInput <- getLine 
            -- I implement readMaybe bcs user may write nothing or some number
            case readMaybe discountInput :: Maybe Float of
                Just discount -> do
                    -- Set specific condtion for discount (1% -- 25%)
                    if discount > 0.0 && discount <= 25.0 then do
                        let updatedItems = applyDiscount discount items
                        let totalCost = calculateTotal updatedItems

                        putStrLn "\n\tCurrent cart items:"
                        outputCart updatedItems
                        putStrLn $ "\nTotal cost after applying discount: " ++ show totalCost
                        menuLoop updatedItems -- Pseudo-updating list

                    else do
                        putStrLn "\n\tInvalid discount. Please enter a correct number"
                        menuLoop items

                Nothing -> do -- Just in case if user won't write discount
                    putStrLn "\n\tInvalid discount. You write nothing."
                    menuLoop items

        -- Exit program
        "0" -> putStrLn "\n\tExiting the program..."
        -- Default case
        _   -> do
            putStrLn "\n\tInvalid option. Please choose a valid option."
            menuLoop items

-- Main function
main :: IO ()
main = menuLoop cart