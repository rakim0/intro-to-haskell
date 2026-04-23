{-
    Simplified version of Wordle:
    where the following rules of hinting are used:
    1. if it's the correct character then 'g'
    2. if it's incorrect but exists somewhere in the string then 'y'
    3. otherwise 'x'

    1
    input : rehab
    output: yyxxx
    input : bulge
    output: xxxxy
    input : ester
    output: ggggg
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
  if i < 0 || i >= length wordList
    then putStrLn "Wrong puzzle number"
    else wordleLoop (wordList !! i) 6

wordleLoop :: (Eq t, Num t) => [Char] -> t -> IO ()
wordleLoop correct i = do
  if i == 0
    then putStrLn ("Game over! The word was: " ++ correct)
    else do
      guess <- getLine
      if length guess /= 5
        then do
          putStrLn "Guess must be exactly 5 letters."
          wordleLoop correct i
        else
          putStrLn (getHint guess correct)
      if guess == correct
        then putStrLn ("Congratulations! You guessed the word: " ++ correct)
        else do
          wordleLoop correct (i - 1)

getHint :: (Eq a) => [a] -> [a] -> [Char]
getHint guess secret = helper guess secret secret
  where
    helper [] [] _ = ""
    helper (g : gs) (c : cs) secret =
      let charHint
            | g == c = 'g'
            | g `elem` secret = 'y'
            | otherwise = 'x'
       in charHint : helper gs cs secret