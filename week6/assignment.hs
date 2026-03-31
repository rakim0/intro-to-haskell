import Data.List
diag :: [[Int]] -> [Int]
diag l = diagHelper l 0

diagHelper :: [[Int]] -> Int -> [Int]
diagHelper [] _ = []
diagHelper (x : xs) i
  | length x <= i = 0 : diagHelper xs (i + 1)
  | otherwise = ((x !! i) + 1) : diagHelper xs (i + 1)

-- define a new function count
count :: (Eq a) => a -> [a] -> Int
count x = length . filter (x ==)

occs :: [Int] -> [Int]
occs [] = []
occs l = map countOccsInL l
  where
    countOccsInL x = count x l

ranks :: [Int] -> [Int]
getRank :: Int -> [Int] -> Int
getRank _ [] = 0
getRank e (x : xs)
  | x < e = notEqualToNext (x : xs) + getRank e xs
  | otherwise = getRank e xs

notEqualToNext [] = 1
notEqualToNext (x : xs)
  | null xs = 1
  | otherwise = if x /= head xs then 1 else 0

ranks l = map getRankForE l
  where
    getRankForE e = getRank e (nub l)

checkIfPowerof5 :: Int -> Int -> Bool
checkIfPowerof5 n x
  | x == n = True
  | x > n = False
  | otherwise = checkIfPowerof5 n (x * 5)

check x = checkIfPowerof5 (x - 2) 1