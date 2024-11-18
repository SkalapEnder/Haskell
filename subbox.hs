import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Map as Map
import Data.Time

-- Structures
data Box = Box {
    boxId :: Int,
    boxName :: String,
    price :: Float,
    contents :: [String],
    stock :: Int
} deriving (Show)

data Subscriber = Subscriber {
    subscriberId :: Int,
    subscriberName :: String,
    email :: String,
    plan :: String
} deriving (Show)

data Subscription = Subscription {
    subscriptionId :: Int,
    subManId :: Int,
    subBoxId :: Int,
    date :: String,
    isDiscountUsed :: Bool
} deriving (Show)

type DiscountMap = Map.Map String Float

-- Data
boxes :: [Box]
boxes =
    [
        Box 0 "Red Box" 25.5 ["Reusable Water Bottle", "Bamboo", "Toothbrush"] 10,
        Box 1 "Blue Box" 45.0 ["Notebook", "Pen", "Sticker"] 15,
        Box 2 "Green Box" 15.75 ["Book", "Toy"] 5,
        Box 3 "Yellow Box" 60.25 ["Candy", "Notebook"] 20,
        Box 4 "Purple Box" 35.0 ["Sticker", "Pen", "Book"] 8
    ]

subscribers :: [Subscriber]
subscribers =
    [ Subscriber 12 "Alice Johnson" "alice.johnson@example.com" "Premium"
    , Subscriber 23 "Bob Smith" "bob.smith@example.com" "Basic"
    , Subscriber 33 "Charlie Brown" "charlie.brown@example.com" "Standard"
    , Subscriber 44 "Diana White" "diana.white@example.com" "Premium"
    , Subscriber 56 "Ethan Black" "ethan.black@example.com" "Basic"
    ]

subscriptions :: [Subscription]
subscriptions = [
    Subscription 0 12 0 "2024-11-01" False, 
    Subscription 1 23 1 "2024-11-02" False, 
    Subscription 2 33 2 "2024-11-03" False, 
    Subscription 3 44 3 "2024-11-04" False, 
    Subscription 4  56 4 "2024-11-05" False 
    ]

discounts :: DiscountMap
discounts = Map.fromList
    [ ("WINTER20", 20.0)
    , ("BLACKFRIDAY", 50.0)
    , ("SUMMER15", 15.0)
    , ("CYBER40", 40.0)
    ]

-- Getter of Free IDs
getFreeBoxId :: Int -> [Box] -> Int
getFreeBoxId i [] = i
getFreeBoxId i (x:xs)
    | i == boxId x = getFreeBoxId (i+1) xs
    | otherwise = i

getFreeSubscriberId :: Int -> [Subscriber] -> Int
getFreeSubscriberId i [] = i
getFreeSubscriberId i (x:xs)
    | i == subscriberId x = getFreeSubscriberId (i+1) xs
    | otherwise = i

getFreeSubscriptionId :: Int -> [Subscription] -> Int
getFreeSubscriptionId i [] = i
getFreeSubscriptionId i (x:xs)
    | i == subscriptionId x = getFreeSubscriptionId (i+1) xs
    | otherwise = i


-- Getter of Objects
getBoxById :: Int -> [Box] -> Maybe Box
getBoxById _ [] = Nothing
getBoxById id (x:xs)
    | id == boxId x = Just x
    | otherwise = getBoxById id xs

getSubscriberById :: Int -> [Subscriber] -> Maybe Subscriber
getSubscriberById _ [] = Nothing
getSubscriberById id (x:xs)
    | id == subscriberId x = Just x
    | otherwise = getSubscriberById id xs

getSubscriptionById :: Int -> [Subscription] -> Maybe Subscription
getSubscriptionById _ [] = Nothing
getSubscriptionById id (x:xs)
    | id == subscriptionId x = Just x
    | otherwise = getSubscriptionById id xs


-- Deleter functions
deleteBoxById :: Int -> [Box] -> [Box]
deleteBoxById targetId = filter (\box -> boxId box /= targetId)

deleteSubscriberById :: Int -> [Subscriber] -> [Subscriber]
deleteSubscriberById targetId = filter (\subman -> subscriberId subman /= targetId)

deleteSubscriptionById :: Int -> [Subscription] -> [Subscription]
deleteSubscriptionById targetId = filter (\subscription -> subscriptionId subscription /= targetId)


-- Output Functions
outputBoxes :: [Box] -> String
outputBoxes [] = "\nThere is no subscribers!"
outputBoxes [x] = "\nID: " ++ show (boxId x) ++
                    "\nName: " ++ boxName x ++
                    "\nPrice: " ++ show (price x) ++
                    "\nContents: " ++ show (contents x) ++
                    "\nStock: " ++ show (isBoxAvailable x) ++ "\n"
outputBoxes (x:xs) =  "\nID: " ++ show (boxId x) ++
                    "\nName: " ++ boxName x ++
                    "\nPrice: " ++ show (price x) ++
                    "\nContents: " ++ show (contents x) ++
                    "\nStock: " ++ show (isBoxAvailable x) ++ "\n" ++ outputBoxes xs

outputBoxesShort :: [Box] -> String
outputBoxesShort [] = "\nThere is no subscribers!"
outputBoxesShort [x] = "\nID: " ++ show (boxId x) ++
                    "\nName: " ++ boxName x ++ "\n" 
outputBoxesShort (x:xs) =  "\nID: " ++ show (boxId x) ++
                        "\nName: " ++ boxName x ++ "\n" ++ outputBoxesShort xs

outputSubscribers :: [Subscriber] -> String
outputSubscribers [] = "\nThere is no subscribers!"
outputSubscribers [x] = "\nID: " ++ show (subscriberId x) ++
                    "\nName: " ++ subscriberName x ++
                    "\nEmail: " ++ email x ++
                    "\nPlan: " ++ plan x ++ "\n"
outputSubscribers (x:xs) = "\nID: " ++ show (subscriberId x) ++
                    "\nName: " ++ subscriberName x ++
                    "\nEmail: " ++ email x ++
                    "\nPlan: " ++ plan x ++ "\n" ++ outputSubscribers xs

outputSubscriptions :: [Subscription] -> String
outputSubscriptions [] = "\nThere is no subscriptions!"
outputSubscriptions [x] = "\nID: " ++ show (subscriptionId x) ++
                    "\nSubscriber ID: " ++ show (subManId x) ++
                    "\nBox ID: " ++ show (subBoxId x) ++
                    "\nDate: " ++ date x ++ "\n"
outputSubscriptions (x:xs) = "\nID: " ++ show (subscriptionId x) ++
                    "\nSubscriber ID: " ++ show (subManId x) ++
                    "\nBox ID: " ++ show (subBoxId x) ++
                    "\nDate: " ++ date x ++ "\n" ++ outputSubscriptions xs


-- Sorting Functions
sortBoxesById :: [Box] -> [Box]
sortBoxesById = sortBy (comparing boxId)

sortSubscribersById :: [Subscriber] -> [Subscriber]
sortSubscribersById = sortBy (comparing subscriberId)

sortSubscriptionsById :: [Subscription] -> [Subscription]
sortSubscriptionsById = sortBy (comparing subscriptionId)

-- Statistics Functions


-- Extra Functions
isBoxAvailable :: Box -> Bool
isBoxAvailable box = stock box > 0

isNotNothing :: Maybe a -> Bool
isNotNothing (Just _) = True
isNotNothing Nothing  = False

stringToDate :: String -> Day
stringToDate = read


-- Menu Section
displayMenu :: IO ()
displayMenu = do
    putStrLn "\n\t==== Subscription Box Service Management System ===="
    putStrLn "\n\tSubscription Section"
    putStrLn "1. Add New Subscription"
    putStrLn "2. Cancel Subscription"
    putStrLn "3. Show all Subscriptions"

    putStrLn "\n\tBox Section"
    putStrLn "4. Add New Box"
    putStrLn "5. Delete New Box"
    putStrLn "6. Show all Boxes"

    putStrLn "\n\tSubscriber Section"
    putStrLn "7. Add New Subscriber"
    putStrLn "8. Delete New Subscriber"
    putStrLn "9. Show all Subscribers"

    putStrLn "\n\tOther Functions"
    putStrLn "10. Apply Seasonal or Promotional Discount"
    putStrLn "11. Check Box Availability"
    putStrLn "12. Monthly Subscription Statistics"
    putStrLn "\n0. Exit"
    putStr "\nPlease choose an option: "

menuLoop :: [Box] -> [Subscriber] -> [Subscription] -> IO ()
menuLoop boxes subscribers subscriptions = do
    displayMenu
    option <- getLine
    case option of
        -- Subscription Section
        -- Add new Subscription
        "1" -> do 
            -- Check if list is not empty
            if not (null subscribers)
                then do
                    -- Check if list of boxes is not empty
                    if not (null boxes)
                        then do
                            putStrLn "\n\t==== List of subscribers ===="
                            putStrLn $ outputSubscribers subscribers
                            putStr "Write ID of subscriber: "
                            t <- getLine
                            let tempSub = getSubscriberById (read t) subscribers
                            
                            putStrLn "\n\t==== List of boxes ===="
                            putStrLn $ outputBoxes boxes
                            putStr "Write ID of box: "
                            j <- getLine
                            let tempBox = getBoxById (read j) boxes

                            
                            if isNotNothing tempSub && isNotNothing tempBox
                                then do
                                    let newSubscription = Subscription (getFreeSubscriptionId 0 subscriptions) (read t) (read j) "2024-11-01" False
                                    let newList = sortSubscriptionsById (newSubscription : subscriptions)
                                    menuLoop boxes subscribers newList

                                else do
                                    putStrLn "\nSubscriber or box is not found!"
                                    menuLoop boxes subscribers subscriptions
                            
                        else do
                            putStrLn "\nThere is no boxes! Please, add new one!"
                            menuLoop boxes subscribers subscriptions

                else do
                    putStrLn "\nThere is no subscribers! Please, add new one!"
                    menuLoop boxes subscribers subscriptions

            
        -- Cancel Subscription
        "2" -> do
            if not (null subscriptions)
                then do
                    putStrLn "\n\t==== List of subscriptions ===="
                    putStrLn $ outputSubscribers subscribers
                    putStr "Write ID of subscription: "
                    t <- getLine
                    let tempSubscr = getSubscriptionById (read t) subscriptions

                    if isNotNothing tempSubscr
                        then do
                            let newSubscriptions = deleteSubscriptionById (read t) subscriptions
                            menuLoop boxes subscribers newSubscriptions

                        else do
                            putStrLn "\nSubscription is not found!"
                            menuLoop boxes subscribers subscriptions

                else do
                    putStrLn "\n\tList of subscriptions is empty!"
                    menuLoop boxes subscribers subscriptions

        -- Show all Subscriptions
        "3" -> do
            putStrLn "\n==== List of subscriptions ===="
            putStrLn $ outputSubscriptions subscriptions
            menuLoop boxes subscribers subscriptions

        -- Box Section
        -- Add New Box (NOT DONE)
        "4" -> do
            putStr "\nWrite name of new box: "
            name <- getLine

            putStr "\nWrite price of new box: "
            price <- getLine

            putStr "Write contents of new box (divided by spaces): "
            contents <- getLine

            putStr "Write count of the box: "
            stock <- getLine


            menuLoop boxes subscribers subscriptions

        -- Delete Box
        "5" -> do
            if not (null boxes)
                then do
                    putStrLn "\n\t==== List of boxes ===="
                    putStrLn $ outputBoxes boxes
                    putStr "Write ID of box: "
                    t <- getLine
                    let tempBox = getBoxById (read t) boxes

                    if isNotNothing tempBox
                        then do
                            let newBoxes = deleteBoxById (read t) boxes
                            menuLoop newBoxes subscribers subscriptions

                        else do
                            putStrLn "\nSubscription is not found!"
                            menuLoop boxes subscribers subscriptions

                else do
                    putStrLn "\n\tList of boxes is empty!"
                    menuLoop boxes subscribers subscriptions

        -- Show all Boxes
        "6" -> do
            putStrLn "\n==== List of boxes ===="
            putStrLn $ outputBoxes boxes
            menuLoop boxes subscribers subscriptions

        -- Subscriber Section
        -- Add new Subscriber (NOT DONE)
        "7" -> do
            putStr "Write name of subscriber: "
            name <- getLine

            putStr "Write email of subscriber: "
            email <- getLine

            putStr "Choose plan of subscription: "
            plan <- getLine

            menuLoop boxes subscribers subscriptions

        -- Delete Subscriber
        "8" -> do
            if not (null subscribers)
                then do
                    putStrLn "\n\t==== List of subscribers ===="
                    putStrLn $ outputSubscribers subscribers
                    putStr "Write ID of subscriber: "
                    t <- getLine
                    let tempBox = getSubscriberById (read t) subscribers

                    if isNotNothing tempBox
                        then do
                            let newSubscribers = deleteSubscriberById (read t) subscribers
                            menuLoop boxes subscribers subscriptions

                        else do
                            putStrLn "\nSubscriber is not found!"
                            menuLoop boxes subscribers subscriptions

                else do
                    putStrLn "\n\tList of subscribers is empty!"
                    menuLoop boxes subscribers subscriptions
            menuLoop boxes subscribers subscriptions

        -- Show all Subscribers
        "9" -> do
            putStrLn "\n==== List of subscribers ===="
            putStrLn $ outputSubscribers subscribers
            menuLoop boxes subscribers subscriptions

        -- Other Functions
        -- Apply Seasonal or Promotional Discount (NOT DONE)
        "10" -> do
            putStr "\nWrite your discount code: "
            code <- getLine

            menuLoop boxes subscribers subscriptions


        -- Check Box Availability
        "11" -> do
            if not (null boxes)
                then do
                    putStrLn "\n\t==== List of boxes ===="
                    putStrLn $ outputBoxesShort boxes
                    putStr "Write ID of box: "
                    t <- getLine
                    let tempBox = getBoxById (read t) boxes

                    if isNotNothing tempBox
                        then do
                            let d = fmap isBoxAvailable tempBox
                            let j = fromMaybe False d 

                            putStrLn $ "Is box available (stock): " ++ show j
                            menuLoop boxes subscribers subscriptions

                        else do
                            putStrLn "\nBox is not found!"
                            menuLoop boxes subscribers subscriptions

                else do
                    putStrLn "\n\tList of boxes is empty!"
                    menuLoop boxes subscribers subscriptions

        -- Monthly Subscription Statistics (NOT DONE)
        "12" -> do
            menuLoop boxes subscribers subscriptions

        -- Exit and Default cases
        "0" -> putStrLn "\n\tExiting the program..."
        _   -> do
            putStrLn "\n\tInvalid option. Please choose a valid option."
            menuLoop boxes subscribers subscriptions

main :: IO ()
main = menuLoop boxes subscribers subscriptions