factorial :: Integer -> Integer 
factorial n 
    | n > 0     = n * factorial (n-1)
    | n == 0    = 1
    | otherwise = error "Negative input"

myGCD :: Integer -> Integer -> Integer 
myGCD a b 
    | a < 0 || b < 0    = myGCD (abs a) (abs b)
    | b == 0            = a 
    | otherwise         = myGCD b (a `mod` b)

isPOF :: Integer -> Bool 
isPOF n | n <= 0        = False 
isPOF 1                 = True 
isPOF n                 = n `mod` 5 == 0 && isPOF (n `div` 5)

isPOF' :: Integer -> Bool 
isPOF' n = go 1 where 
    go :: Integer -> Bool
    go p  
        | p < n     = go (5*p)
        | p == n    = True 
        | otherwise = False

largestDiv :: Integer -> Integer 
largestDiv n = n `div` divSearch 2 where 
    divSearch :: Integer -> Integer 
    -- divSearch m is the smallest number >= m that divides n
    divSearch m = if n `mod` m == 0 then m else divSearch (m+1)

intLen :: Integer -> Integer 
intLen n
    | n < 0     = error "No negative inputs!"
    | n < 10    = 1
    | otherwise = intLen (n `div` 10) + 1

intRev :: Integer -> Integer 
intRev n 
    | n < 0     = error "No negative inputs allowed!"
    | otherwise = go 0 n where 
    go a n      = if n == 0 then a else go (10*a + r) q where 
        (q,r)   = n `divMod` 10

power :: Integer -> Int -> Integer
power x 0 = 1
power x n = x * power x (n-1)

fPow :: Integer -> Int -> Integer 
fPow x 0 = 1
fPow x n = if even n then y * y else y * y * x where 
    y = fPow x (div n 2)

fib' :: Int -> (Integer, Integer)
fib' 0 = (0, 1)
fib' 1 = (1, 1)
fib' n = (y, x + y) where 
    (x,y) = fib' (n-1)

fib :: Int -> Integer
fib n = fst (fib' n)
