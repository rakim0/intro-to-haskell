{-
    Simplified version of Wordle:
    where the following rules of hinting are used:
    1. if it's the correct character then 'g'
    2. if it's incorrect but exists somewhere in the string then 'y'
    3. otherwise 'x'

    Example:
    1

-}

wordList :: [String]
wordList =
  [ "rehab",
    "ester",
    "craze",
    "bulge"
  ]

main :: IO ()
main = do
  i <- readLn :: IO Int
  if (i < 0 || i >= 3)
    then putStrLn "Wrong puzzle number"
    else wordleLoop (wordList !! i) 6

wordleLoop correct i = do
  if i == 0
    then putStr ("Game over! The word was: " ++ correct)
    else do
      guess <- getLine
      if (length guess) /= 5
        then do
            putStrLn("Guess must be exactly 6 letters.")
            wordleLoop correct i
      else 
        putStrLn (getHint guess correct)
      if guess == correct
        then putStr ("Congratulations! You guessed the word: " ++ correct)
        else do 
            wordleLoop correct (i-1)



getHint guess secret = helper guess secret secret 
    where 
        helper [] [] _ = ""
        helper (g:gs) (c:cs) secret = 
            let charHint = if g == c
                then 'g'
                else if g `elem` secret 
                        then 'y'
                        else 'x'
            in charHint:helper gs cs secret