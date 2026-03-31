gcd1 :: Int -> Int -> Int
gcd1 a b  
    | a < 0 || b < 0 = error "Invalid Input. gcd1 only works with positive numbers"
    | b == 0 = a
    | otherwise = gcd1 b (a `mod` b)

isPOF :: Integer -> Bool
isPOF n
    | n <= 0 = False
    | n == 1 = True
    | otherwise = n `mod` 5 == 0 && isPOF(n`div`5)

isPOF2 :: Integer -> Integer -> Bool
isPOF2 p n
    | p == n = True
    | p < n = isPOF2 (p*5) n
    | otherwise = False

-- smallest divisor of number n, which is not 1
smallestDivisor :: Integer -> Integer 
smallestDivisor p = 
    let search n
            | p `mod` n == 0 = n
            | otherwise = search (n+1)
    in search 2

-- largestDivisor :: Int -> Int
-- largestDivisor p = divSearch (p-1)
--     where divSearch n
--             | p `mod` n == 0 = n
--             | otherwise =  divSearch (n-1)

largestDivisor :: Integer -> Integer
largestDivisor n = div n $ smallestDivisor n

lengthOfInteger :: Integer -> Integer
lengthOfInteger n 
    | n < 0 = error "too small"
    | n < 10 = 1
    | otherwise = 1+lengthOfInteger(n`div`10)

reverseNum :: Integer -> Integer
reverseNum n = helper n 0
    where helper n m
            | n == 0 = m
            | otherwise = helper ( n `div` 10) (m*10+(n`mod`10))