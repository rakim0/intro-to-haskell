main :: IO ()
main = putStr "Hello world!\n"

main2 :: IO ()
main2 = putStrLn "Hello world!"

main3 :: IO ()
main3 = do 
    putStrLn "Hello!"
    putStrLn "What's your name?"

fibs :: [Integer]
fibs = 0:1:zipWith (+) fibs (drop 1 fibs)

main4 :: IO ()
main4 = do 
        print (fibs!!5)
        print (fibs!!10)

main5 :: IO ()
main5 = do 
    putStrLn "Please enter a name!"
    n <- getLine 
    putStrLn ("Hi " ++ n ++ ", welcome to Haskell!")

main6 :: IO ()
main6 = do 
    putStrLn "Enter a number!"
    n <- readLn :: IO Int 
    putStrLn $ "The Fibonacci number at index " ++ show n ++ " = " ++ show (fibs!!n)

main7 :: IO ()
main7 = do 
    putStrLn "Enter something"
    s <- getLine 
    doOften 10 (putStrLn s) 

doOften :: Int -> IO () -> IO ()
doOften 0 a = return () 
doOften n a = do {a; doOften (n-1) a;} 

myGetLine :: IO String 
myGetLine = do 
    c <- getChar 
    if c == '\n' 
        then 
            return "" 
        else do 
            cs <- myGetLine 
            return (c:cs)

main8 :: IO ()
main8 = do 
    putStrLn "Enter a non-negative index, or a negative number if you want to quit!"
    n <- readLn :: IO Int 
    if n < 0 
        then do 
            putStrLn "Thank you! Good bye!" 
            return ()
        else do 
            putStrLn $ "The Fibonacci number at index " ++ show n ++ " = " ++ show (fibs!!n)
            main8 
