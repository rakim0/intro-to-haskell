import Data.List ( sort )

main :: IO ()
main = do 
    listActions []

myReverse :: [Int] -> [Int]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x] 

listActions :: [Int] -> IO ()
listActions xs = do 
    cmd <- getLine 
    let l = words cmd
    case l!!0 of 
        "add" -> do 
            let x = read (l!!1) :: Int
            let newList = x:xs
            putStrLn("Current list = "++show newList)
            listActions newList

        "del" -> do
            let newList =  if (length xs) > 0 then tail xs else []
            putStrLn("Current list = "++show newList)
            listActions newList

        "srt" -> do
            let newList = sort xs
            putStrLn("Current list = "++show newList)
            listActions newList

        "rev" -> do
            let newList = myReverse xs
            putStrLn("Current list = "++show newList)
            listActions newList
                

        "bye" -> do 
            putStr "Exiting! Good bye!"

        otherwise -> do 
            putStrLn "Invalid operation!"
            listActions xs
