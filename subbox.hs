import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Map as Map
import Data.Time

-- Structures (Both)
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
    plan :: Int
} deriving (Show)

data Subscription = Subscription {
    subscriptionId :: Int,
    subManId :: Int,
    subBoxId :: Int,
    date :: String,
    discount :: Float,
    isDiscountUsed :: Bool
} deriving (Show)

-- (Yeldos)
type DiscountMap = Map.Map String Float

discounts :: DiscountMap
discounts = Map.fromList
    [ ("WINTER20", 20.0)
    , ("BLACKFRIDAY", 50.0)
    , ("SUMMER15", 15.0)
    , ("CYBER40", 40.0)
    ]

-- Data (Both)
boxes :: [Box]
boxes =
    [
        Box 0 "Red Box" 25.0 ["Reusable Water Bottle", "Bamboo", "Toothbrush"] 10,
        Box 1 "Blue Box" 45.0 ["Notebook", "Pen", "Sticker"] 15,
        Box 2 "Green Box" 16.0 ["Book", "Toy"] 5,
        Box 3 "Yellow Box" 60.0 ["Candy", "Notebook"] 20,
        Box 4 "Purple Box" 35.0 ["Sticker", "Pen", "Book"] 8
    ]

subscribers :: [Subscriber]
subscribers =
    [ Subscriber 12 "Alice Johnson" "alice.johnson@example.com" 1
    , Subscriber 23 "Bob Smith" "bob.smith@example.com" 2
    , Subscriber 33 "Charlie Brown" "charlie.brown@example.com" 1
    , Subscriber 44 "Diana White" "diana.white@example.com" 1
    , Subscriber 56 "Ethan Black" "ethan.black@example.com" 2
    ]

subscriptions :: [Subscription]
subscriptions = [
    Subscription 0 12 0 "2024-11-21" 25.0 False,
    Subscription 1 23 1 "2024-11-22" 45.0 False,
    Subscription 2 33 2 "2024-11-23" 16.0 False,
    Subscription 3 44 3 "2024-11-24" 60.0 False,
    Subscription 4 56 4 "2024-11-25" 35.0 False,
    Subscription 5 56 2 "2024-11-26" 16.0 False
    ]

--          Functions
-- Getter of Free IDs (Alisher)
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


-- Getter of Objects (Alisher)
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


-- Deleter functions (Extra) (Yeldos)
deleteBoxById :: Int -> [Box] -> [Box]
deleteBoxById targetId = filter (\box -> boxId box /= targetId)

deleteSubscriberById :: Int -> [Subscriber] -> [Subscriber]
deleteSubscriberById targetId = filter (\subman -> subscriberId subman /= targetId)

deleteSubscriptionById :: Int -> [Subscription] -> [Subscription]
deleteSubscriptionById targetId = filter (\subscription -> subscriptionId subscription /= targetId)


-- Output Functions (Extra) (Alisher)
outputBoxes :: [Box] -> String
outputBoxes [] = "\nThere is no subscribers!"
outputBoxes [x] = "\nID: " ++ show (boxId x) ++
                    "\nName: " ++ boxName x ++
                    "\nPrice: " ++ show (price x) ++ "$" ++
                    "\nContents: " ++ show (contents x) ++
                    "\nStock: " ++ show (isBoxAvailable x) ++ "\n"
outputBoxes (x:xs) =  "\nID: " ++ show (boxId x) ++
                    "\nName: " ++ boxName x ++
                    "\nPrice: " ++ show (price x) ++ "$" ++
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
                    "\nPlan: " ++ outputPlan (plan x) ++ "\n"
outputSubscribers (x:xs) = "\nID: " ++ show (subscriberId x) ++
                    "\nName: " ++ subscriberName x ++
                    "\nEmail: " ++ email x ++
                    "\nPlan: " ++ outputPlan (plan x) ++ "\n" ++ outputSubscribers xs

outputSubscriptions :: [Subscription] -> String
outputSubscriptions [] = "\nThere is no subscriptions!"
outputSubscriptions [x] = "\nID: " ++ show (subscriptionId x) ++
                    "\nSubscriber ID: " ++ show (subManId x) ++
                    "\nBox ID: " ++ show (subBoxId x) ++
                    "\nDiscounted price: " ++ show (discount x) ++ "$" ++
                    "\nDate: " ++ date x ++ "\n"
outputSubscriptions (x:xs) = "\nID: " ++ show (subscriptionId x) ++
                    "\nSubscriber ID: " ++ show (subManId x) ++
                    "\nBox ID: " ++ show (subBoxId x) ++
                    "\nDiscounted price: " ++ show (discount x) ++ "$" ++
                    "\nDate: " ++ date x ++ "\n" ++ outputSubscriptions xs


-- Sorting Functions (Extra) (Both)
sortBoxesById :: [Box] -> [Box]
sortBoxesById = sortBy (comparing boxId)

sortSubscribersById :: [Subscriber] -> [Subscriber]
sortSubscribersById = sortBy (comparing subscriberId)

sortSubscriptionsById :: [Subscription] -> [Subscription]
sortSubscriptionsById = sortBy (comparing subscriptionId)

-- Discount section (Yeldos)
applyDiscount :: Int -> Float -> [Subscription] -> [Subscription]
applyDiscount targetId disc = map (\sub -> 
        if subscriptionId sub == targetId
            then sub { discount = discount sub * (1 - (disc / 100.0)), isDiscountUsed = True }
            else sub)

isBoxAvailable :: Box -> Bool
isBoxAvailable box = stock box > 0

-- Extra Functions 
isNotNothing :: Maybe a -> Bool
isNotNothing (Just _) = True
isNotNothing Nothing  = False

outputPlan :: Int -> String
outputPlan x = case x of 1 -> "Monthly"
                         2 -> "Quartely"

reduceStock :: Int -> [Box] -> [Box]
reduceStock targetId = map (\box -> 
        if boxId box == targetId
            then box { stock = stock box - 1}
            else box) 

-- Statistics Function (Yeldos)
subscriptionsPerBox :: [Box] -> [Subscription] -> [(Int, Int)]
subscriptionsPerBox boxs subscrips = map (\box -> (boxId box, length (filter (\sub -> subBoxId sub == boxId box) subscrips))) boxs

outputSubsPerBox :: [(Int, Int)] -> IO()
outputSubsPerBox = mapM_ outputTuple
    where outputTuple tup = putStrLn $ show (fst tup) ++ " ID box - " ++ show (snd tup) ++ " subscriptions" 


--          Menu Section
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
    putStrLn "10. Apply Promotional Discount"
    putStrLn "11. Monthly Subscription Statistics"
    putStrLn "\n0. Exit"
    putStr "\nPlease choose an option: "

menuLoop :: [Box] -> [Subscriber] -> [Subscription] -> IO ()
menuLoop boxes subscribers subscriptions = do
    displayMenu
    option <- getLine
    case option of

        -- ##### (Alisher) #####
        --      Subscription Section
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
                            let subId = read t :: Int
                            let tempSub = getSubscriberById subId subscribers

                            putStrLn "\n\t==== List of boxes ===="
                            putStrLn $ outputBoxes boxes
                            putStr "Write ID of box: "
                            j <- getLine
                            let tempBox = getBoxById (read j) boxes

                            -- There we proving that they are found
                            if isNotNothing tempSub && isNotNothing tempBox
                                then do
                                    let isBoxAccessible = fromMaybe False (fmap isBoxAvailable tempBox)

                                    if isBoxAccessible -- If stock of box isn't 0
                                        then do
                                            let boxPrice = price $ fromJust tempBox
                                            let newSubscription = Subscription (getFreeSubscriptionId 0 subscriptions) (read t) (read j) "2024-11-25" boxPrice False
                                            let newSubs = sortSubscriptionsById (newSubscription : subscriptions)
                                            let newBoxes = reduceStock (read j) boxes

                                            putStrLn "\n\t\tNew subscription is added!\n"
                                            menuLoop newBoxes subscribers newSubs
                                        else do
                                            putStrLn "\n\t\tBox is not available! Sorry!\n"
                                            menuLoop boxes subscribers subscriptions

                                else do
                                    putStrLn "\n\t\tSubscriber or box is not found!\n"
                                    menuLoop boxes subscribers subscriptions

                        else do
                            putStrLn "\n\t\tThere is no boxes! Please, add new one!\n"
                            menuLoop boxes subscribers subscriptions

                else do
                    putStrLn "\n\t\tThere is no subscribers! Please, add new one!\n"
                    menuLoop boxes subscribers subscriptions

        -- Cancel Subscription
        "2" -> do
            if not (null subscriptions)
                then do
                    putStrLn "\n\t==== List of subscriptions ===="
                    putStrLn $ outputSubscriptions subscriptions
                    putStr "Write ID of subscription: "
                    t <- getLine
                    let subId = read t :: Int
                    let tempSubscr = getSubscriptionById subId subscriptions

                    if isNotNothing tempSubscr
                        then do
                            -- Main actions here!
                            let newSubscriptions = deleteSubscriptionById subId subscriptions
                            putStrLn "\nThe subscription is deleted!\n"
                            menuLoop boxes subscribers newSubscriptions

                        else do
                            putStrLn "\nSubscription is not found!\n"
                            menuLoop boxes subscribers subscriptions

                else do
                    putStrLn "\n\tList of subscriptions is empty!\n"
                    menuLoop boxes subscribers subscriptions

        -- Show all Subscriptions
        "3" -> do
            putStrLn "\n==== List of subscriptions ===="
            putStrLn $ outputSubscriptions subscriptions
            menuLoop boxes subscribers subscriptions


        --          Box Section
        -- Add New Box
        "4" -> do
            putStr "\nWrite name of new box: "
            name <- getLine

            putStr "\nWrite price of new box: "
            price <- getLine

            putStr "\nWrite contents of new box (divided by spaces): "
            contents <- getLine
            let content = words contents

            putStr "\nWrite count of the box: "
            stock <- getLine

            if name /= ""
                then do
                    if price /= ""
                        then do
                            if contents /= ""
                                then do
                                    if read stock > 0
                                        then do
                                            -- Main actions here!
                                            let newBox = Box (getFreeBoxId 0 boxes) name (read price) content (read stock)
                                            let newList = sortBoxesById (newBox : boxes)
                                            putStrLn "\n\t\tNew box is added!\n"
                                            menuLoop newList subscribers subscriptions
                                        else do
                                            putStrLn "\n\t\tStock cannot be lower than 1!\n"
                                            menuLoop boxes subscribers subscriptions
                                else do
                                    putStrLn "\n\t\tThere is nothing in box! Please, write smth!\n"
                                    menuLoop boxes subscribers subscriptions     
                        else do
                            putStrLn "\n\t\tThere is no price! Please, write smth!\n"
                            menuLoop boxes subscribers subscriptions
                else do
                    putStrLn "\n\t\tThere is no name! Please, write smth!\n"
                    menuLoop boxes subscribers subscriptions

        -- Delete Box
        "5" -> do
            if not (null boxes)
                then do
                    putStrLn "\n\t==== List of boxes ===="
                    putStrLn $ outputBoxes boxes
                    putStr "Write ID of box: "
                    t <- getLine
                    let subId = read t :: Int
                    let tempBox = getBoxById subId boxes

                    if isNotNothing tempBox
                        then do
                            -- Main actions here!
                            let newBoxes = deleteBoxById subId boxes
                            putStrLn "\nThe box is deleted!\n"
                            menuLoop newBoxes subscribers subscriptions

                        else do
                            putStrLn "\nSubscription is not found!\n"
                            menuLoop boxes subscribers subscriptions

                else do
                    putStrLn "\n\tList of boxes is empty!"
                    menuLoop boxes subscribers subscriptions

        -- Show all Boxes
        "6" -> do
            putStrLn "\n\t==== List of boxes ===="
            putStrLn $ outputBoxes boxes
            menuLoop boxes subscribers subscriptions


        --       Subscriber Section
        -- Add new Subscriber
        "7" -> do
            putStr "Write name of subscriber: "
            name <- getLine

            putStr "Write email of subscriber: "
            email <- getLine


            putStrLn "\n1 -> Monthly (Default)"
            putStrLn "2 -> Quarterly"
            putStrLn "3 -> Yearly"
            putStr "Choose plan of subscription: "
            j <- getLine
            let plan = read j :: Int

            if plan > 0 && plan < 4
                then do
                    let newSubscriber = Subscriber (getFreeSubscriberId 0 subscribers) name email plan
                    let newList = sortSubscribersById (newSubscriber : subscribers)
                    putStrLn "\n\t\tNew subscriber is added!\n"
                    menuLoop boxes newList subscriptions
                else do
                    putStrLn "\n\t\tChosen plan is unknown! Take another one!\n"
                    menuLoop boxes subscribers subscriptions


        -- Delete Subscriber
        "8" -> do
            if not (null subscribers)
                then do
                    putStrLn "\n\t==== List of subscribers ===="
                    putStrLn $ outputSubscribers subscribers
                    putStr "Write ID of subscriber: "
                    t <- getLine
                    let subId = read t :: Int
                    let tempBox = getSubscriberById subId subscribers

                    if isNotNothing tempBox
                        then do
                            -- Main actions here!
                            let newSubscribers = deleteSubscriberById subId subscribers
                            putStrLn "\nThe subscriber is deleted!"
                            menuLoop boxes newSubscribers subscriptions

                        else do
                            putStrLn "\nSubscriber is not found!"
                            menuLoop boxes subscribers subscriptions

                else do
                    putStrLn "\n\tList of subscribers is empty!"
                    menuLoop boxes subscribers subscriptions

        -- Show all Subscribers
        "9" -> do
            putStrLn "\n\t==== List of subscribers ===="
            putStrLn $ outputSubscribers subscribers
            menuLoop boxes subscribers subscriptions
        -- ##### (Alisher) #####


        -- ##### (Yeldos) #####
        -- Apply Promotional Discount
        "10" -> do
            if not (null subscriptions)
                then do
                    putStrLn "\n\t==== List of subscriptions ===="
                    putStrLn $ outputSubscriptions subscriptions
                    putStr "Write ID of subscription: "
                    t <- getLine
                    let subId = read t :: Int

                    -- Get subscription object to check if he exists
                    let tempSubscr = getSubscriptionById subId subscriptions

                    -- Check if list is not empty
                    if isNotNothing tempSubscr
                        then do
                            -- Check if subscription is not discounted
                            if not (isDiscountUsed $ fromJust tempSubscr)
                                then do
                                    putStr "\nWrite your discount code: "
                                    code <- getLine
                                    -- Check if code is actual discount
                                    if Map.member code discounts
                                        then do
                                            -- Main actions here!
                                            let discount = fromJust (Map.lookup code discounts)
                                            let newSubs = applyDiscount subId discount subscriptions
                                            putStrLn "\n\t\tThe discount price is updated!\n"
                                            menuLoop boxes subscribers newSubs
                                        else do
                                        putStrLn "\n\t\tThe discount is incorrect!\n"
                                        menuLoop boxes subscribers subscriptions    
                                else do
                                    putStrLn "\n\t\tSorry! You used discount!\n"
                                    menuLoop boxes subscribers subscriptions
                        else do
                            putStrLn "\n\t\tSubscription is not found!\n"
                            menuLoop boxes subscribers subscriptions
                else do
                    putStrLn "\n\t\tList of subscriptions is empty!\n"
                    menuLoop boxes subscribers subscriptions

        -- Monthly Statistics
        "11" -> do
            putStrLn "\n\tStatistics of this month"

            let totalRevenue = sum (map discount subscriptions)
            let avgRevenue = totalRevenue / fromIntegral (length subscriptions)

            putStrLn $ "Total revenue: " ++ show totalRevenue 
            putStrLn $ "Average per subscriber: " ++ show avgRevenue
            
            putStrLn "\n\tSubscriptions per box"
            outputSubsPerBox $ subscriptionsPerBox boxes subscriptions
            menuLoop boxes subscribers subscriptions


        -- Exit and Default cases
        "0" -> putStrLn "\n\tExiting the program..."
        _   -> do
            putStrLn "\n\t\tInvalid option. Please choose a valid option.\n"
            menuLoop boxes subscribers subscriptions


main :: IO ()
main = menuLoop boxes subscribers subscriptions