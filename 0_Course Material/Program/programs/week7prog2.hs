import System.IO ( hFlush, stdout, isEOF )
import Control.Monad ( forever, when, replicateM )
import System.Exit ( exitSuccess )
import System.Random ( randomIO, randomRIO )

main0 :: IO ()
main0 = do 
    s <- getLine 
    printOften 10 s 

printOften :: Int -> String -> IO ()
printOften 0 str = return () 
printOften n str = do 
    putStrLn str 
    printOften (n-1) str

myGetLine :: IO String 
myGetLine = do 
    c <- getChar 
    if c == '\n' 
    then return "" 
    else do 
        cs <- myGetLine 
        return (c:cs)

fibs :: [Integer]
fibs = 0:1: zipWith (+) fibs (drop 1 fibs)

main1 :: IO ()
main1 = do 
    putStr "Enter a non-negative index: "; hFlush stdout 
    n <- readLn :: IO Int 
    if n < 0 
        then do 
            putStrLn "Thank you! Good bye!" 
            return ()
        else do 
            putStrLn $ "The Fibonacci number at index " ++ show n ++ " = " ++ show (fibs!!n)
            main1 

main2 :: IO ()
main2 = nTimes 10 $ do 
    inp <- getLine 
    putStrLn inp 

nTimes :: Int -> IO () -> IO ()
nTimes 0 _ = return () 
nTimes n a = do {a; nTimes (n-1) a;}

main3 :: IO ()
main3 = do 
    xs <- readIntList 
    print xs 

readIntList :: IO [Int]
readIntList = do 
    exitCond <- isEOF 
    if exitCond
    then return []
    else do 
        x <- readLn :: IO Int 
        xs <- readIntList 
        return (x:xs)

main4 :: IO ()
main4 = forever $ do 
    exitCond <- isEOF 
    when exitCond exitSuccess 
    xs <- readLn :: IO [Int]
    print (reverse xs)

main5 :: IO ()
main5 = do 
    names <- replicateM 5 getLine 
    print names 

randInts :: IO [Int]
randInts = replicateM 100 (randomRIO (-100, 99))

randString :: IO String 
randString = replicateM 100 (randomRIO ('a', 'z'))

main :: IO ()
main = main1
