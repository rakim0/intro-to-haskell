import Data.Char (digitToInt, toUpper)
import Data.List

mySum :: [Int] -> Int
mySum [] = 0
mySum (x : xs) = x + mySum xs

mySum2 :: [Int] -> Int
mySum2 = foldr (+) 0

sumLen :: [[a]] -> Int
sumLen [] = 0
sumLen (l : ls) = length l + sumLen ls

sumLen3 :: [[a]] -> Int
sumLen3 = foldr f 0
  where
    f l x = length l + x

sumLen2 :: [[a]] -> Int
sumLen2 ls = sum (map length ls)

ls :: [Integer]
ls = [2, 3, 4, 5]

fs :: [Integer -> Integer]
fs = map (+) ls

xs :: [Integer]
xs = map (\f -> f 15) fs

allEvens :: [Integer] -> [Integer]
allEvens = filter even

sqrEvens :: [Integer] -> [Integer]
sqrEvens = map (^ 2) . filter even

capVows :: [Char] -> [Char]
capVows = map toUpper . filter (`elem` "aeiouAEIOU")

myFilter, myTakeWhile, myDropWhile :: (a -> Bool) -> [a] -> [a]
myFilter p [] = []
myFilter p (x : xs) =
  if p x
    then x : myFilter p xs
    else myFilter p xs
myTakeWhile p [] = []
myTakeWhile p (x : xs) = if p x then x : myTakeWhile p xs else []
myDropWhile p [] = []
myDropWhile p (x : xs) = if p x then myDropWhile p xs else x : xs

myMax :: [Int] -> Int
myMax [x] = x
myMax (x : xs) = max x (myMax xs)

mySumr, mySuml :: [Int] -> Int
mySumr [] = 0
mySumr (x : xs) = x + mySumr xs
mySuml = go 0
  where
    go acc [] = acc
    go acc (x : xs) = go (acc + x) xs

strToNum1 :: String -> Integer
strToNum1 = go 0
  where
    go acc "" = acc
    go acc (c : cs) = go (10 * acc + toInteger (digitToInt c)) cs

strToNum :: String -> Integer
strToNum = foldl (\acc c -> 10 * acc + toInteger (digitToInt c)) 0

bigSum :: Integer
bigSum = foldl' (+) 0 [1 .. 100000000]

biggerSum :: Integer
biggerSum = foldl' (+) 0 [1 .. 1000000000]

-- An instance of scanr
func :: (Int -> Int -> Int) -> Int -> [Int] -> [Int]
func f v [] = [v]
func f v (x : xs) = f x y : ys
  where
    ys@(y : _) = func f v xs