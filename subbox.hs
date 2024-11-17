data Box = Box {
    boxId :: Int,
    boxName :: String,
    price :: Float,
    contents :: [String],
    stock :: Int
}

data Subscriber = Subscriber {
    subManId :: Int,
    subManName :: String,
    email :: String,
    plan :: String
}

data Subscription = Subscription {
    subScrId :: Int,
    subScrInfo :: Subscriber,
    subScrBox :: Box,
    discount :: Float,
    date :: String
}

boxes :: [Box]
boxes = 
    [
        Box 1 "Red Box" 25.5 ["Toy", "Candy"] 10,
        Box 2 "Blue Box" 45.0 ["Notebook", "Pen", "Sticker"] 15,
        Box 3 "Green Box" 15.75 ["Book", "Toy"] 5,
        Box 4 "Yellow Box" 60.25 ["Candy", "Notebook"] 20,
        Box 5 "Purple Box" 35.0 ["Sticker", "Pen", "Book"] 8
    ]

subscribers :: [Subscriber]
subscribers =
    [ Subscriber 1 "Alice Johnson" "alice.johnson@example.com" "Premium"
    , Subscriber 2 "Bob Smith" "bob.smith@example.com" "Basic"
    , Subscriber 3 "Charlie Brown" "charlie.brown@example.com" "Standard"
    , Subscriber 4 "Diana White" "diana.white@example.com" "Premium"
    , Subscriber 5 "Ethan Black" "ethan.black@example.com" "Basic"
    ]

subscriptions :: [Subscription]
subscriptions =
    [ Subscription 1 (subscribers !! 0) (boxes !! 0) 10.0 "2024-11-01"
    , Subscription 2 (subscribers !! 1) (boxes !! 1) 5.0 "2024-11-02"
    , Subscription 3 (subscribers !! 2) (boxes !! 2) 15.0 "2024-11-03"
    , Subscription 4 (subscribers !! 3) (boxes !! 3) 20.0 "2024-11-04"
    , Subscription 5 (subscribers !! 4) (boxes !! 4) 0.0 "2024-11-05"
    ]


getFreeBoxId :: Int -> [Box] -> Int
getFreeBoxId i [] = i
getFreeBoxId i (x:xs)
    | i == boxId x = getFreeBoxId (i+1) xs
    | otherwise = i

getFreeSubscriberId :: Int -> [Subscriber] -> Int
getFreeSubscriberId i [] = i
getFreeSubscriberId i (x:xs)
    | i == subManId x = getFreeSubscriberId (i+1) xs
    | otherwise = i


getBoxById :: Int -> [Box] -> Maybe Box
getBoxById _ [] = Nothing
getBoxById id (x:xs)
    | id == boxId x = Just x
    | otherwise = getBoxById id xs

getSubscriberById :: Int -> [Subscriber] -> Maybe Subscriber
getSubscriberById _ [] = Nothing
getSubscriberById id (x:xs)
    | id == subManId x = Just x
    | otherwise = getSubscriberById id xs


displayMenu :: IO ()
displayMenu = do
    putStrLn "\n\t==== Subscription Box Service Management System ===="
    putStrLn "1. Add New Subscription"
    putStrLn "2. Cancel Subscription"
    putStrLn "3. Apply Seasonal or Promotional Discount"
    putStrLn "4. Check Box Availability"
    putStrLn "5. Monthly Subscription Statistics"
    putStrLn "0. Exit"
    putStr "\nPlease choose an option: "

menuLoop :: [Box] -> [Subscriber] -> [Subscription] -> IO ()
menuLoop boxes subscribers subscriptions = do
    displayMenu
    option <- getLine
    case option of
        "1" -> do
            putStr "Write ID of subscriber: "
            t <- getLine
            let tempSub = getSubscriberById (read t) subscribers
            
            putStr "Write ID of box: "
            j <- getLine
            let tempBox = getBoxById (read j) boxes
             
            menuLoop boxes subscribers subscriptions

        "2" -> do
            menuLoop boxes subscribers subscriptions

        "3" -> do
            menuLoop boxes subscribers subscriptions

        "4" -> do
            menuLoop boxes subscribers subscriptions

        "5" -> do
            menuLoop boxes subscribers subscriptions

        "0" -> putStrLn "\n\tExiting the program..."
        _   -> do
            putStrLn "\n\tInvalid option. Please choose a valid option."
            menuLoop boxes subscribers subscriptions

main :: IO ()
main = menuLoop boxes subscribers subscriptions