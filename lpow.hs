largestPower:: Int -> Int -> Integer


largestPower num limit 
    | (num == 1 || num == 0) = num
    | num > limit = -1
    | otherwise = findPower num 1 limit

findPower num cur limit 
    | cur > limit = cur `div` num
    | otherwise = findPower num (cur*num) limit


largestPower' b n = last [b^x | x [0..], b^x <= n]

