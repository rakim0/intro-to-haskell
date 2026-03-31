{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
checkPalindrome :: String -> Bool
checkPalindrome "" = True
checkPalindrome s
  | length s == 1 = True
  | otherwise = (head s == last s) && checkPalindrome (tail (init s))

checkPalindromeInt :: Int -> Bool
checkPalindromeInt x = checkPalindrome (show x)

closest_pal :: Int -> Int
closest_pal n
  | checkPalindromeInt (n + 1) = n + 1
  | otherwise = closest_pal (n + 1)

-- longest descending suffix
-- [1,2,3,4] => [4]
-- [4,3,2,1] => [4,3,2,1]
-- [4,3,5,2,1] =>
lds :: [Int] -> [Int]
lds x = ldsHelper x []

ldsHelper :: [Int] -> [Int] -> [Int]
ldsHelper [] b = b
ldsHelper a [] = ldsHelper (init a) [last a]
ldsHelper a (y : ys)
  | last a > y = ldsHelper (init a) (last a : y : ys)
  | last a <= y = y : ys