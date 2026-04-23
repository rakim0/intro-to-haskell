import Data.Array ((!), listArray)

las :: [Int] -> [Int]
las as = longest [lasArr!i | i <- [0..n]] where
    n = length as
    aArr = listArray (0,n) as
    lasArr = listArray(0, n) [lasMemo i | i <- [0..n]]
    lasMemo i 
        | i == n = []
        | otherwise = (aArr!i):longest [lasArr!j | j <- [i+1..n], ascend (aArr!i) (lasArr!j)]
    ascend x [] = True
    ascend x (y:_) = x < y
    longest = foldr(\l acc -> if length l >= length acc then l else acc) []