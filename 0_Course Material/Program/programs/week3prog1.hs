addOne :: [Integer] -> [Integer]
-- addOne l adds 1 to every element of l 
addOne l = if null l 
    then []
    else let x:xs= l in x + 1 : addOne xs

myLength :: [Integer] -> Integer 
myLength l = if null l 
    then 0
    else let _:xs = l in 1 + myLength xs

myLen :: [Integer] -> Integer
myLen []        = 0
myLen (_:xs)    = 1 + myLen xs

addAtEnd :: [Integer] -> Integer -> [Integer]
-- addAtEnd l x places x at the end of l
addAtEnd []     x = [x]
addAtEnd (y:ys) x = y:addAtEnd ys x

append1, append2 :: [Integer] -> [Integer] -> [Integer]
-- append l1 l2 adds the entire list l2 at the end of l1
-- append [1,2,3] [4,5,6] = [1,2,3,4,5,6]
append1 l1 []       = l1 
append1 l1 (x:xs)   = append1 (addAtEnd l1 x) xs

append2 [] l2       = l2 
append2 (x:xs) l2   = x:append2 xs l2 

valueAtPos :: Int -> [a] -> a 
valueAtPos n _ 
    | n < 0         = error "Negative index not allowed!"
valueAtPos _ []     = error "Index too large!"
valueAtPos 0 (x:_)  = x
valueAtPos n (x:xs) = valueAtPos (n-1) xs 

(#) :: [a] -> Int -> a 
(#) _ n
    | n < 0     = error "Negative index not allowed!"
(#) [] _        = error "Index too large!"
(x:_) # 0       = x
(x:xs) # n      = xs # (n-1) 

myRev :: [a] -> [a]
myRev []        = []
myRev (x:xs)    = myRev xs ++ [x]

-- revInto :: [a] -> [a] -> [a]
-- revInto acc l = myRev l ++ acc 

{-
revInto acc [] = myRev [] ++ acc = [] ++ acc = acc 
revInto acc (x:xs) = myRev (x:xs) ++ acc 
    = (myRev xs ++ [x]) ++ acc
    = myRev xs ++ ([x] ++ acc)
    = myRev xs ++ (x:acc)
    = revInto (x:acc) xs 
-} 

revInto :: [a] -> [a] -> [a]
revInto acc []      = acc 
revInto acc (x:xs)  = revInto (x:acc) xs 

-- If l is of length n, how much time does it take to evaluate 
--          revInto acc l ?

fastRev :: [a] -> [a]
fastRev = revInto []

takeDrop :: Int -> [a] -> ([a], [a])
takeDrop _ []       = ([], [])
takeDrop n (x:xs)   = if n <= 0 
    then ([], x:xs) 
    else let (ys, zs) = takeDrop (n-1) xs in
        (x:ys, zs) 
divisors :: Integer -> [Integer]
divisors x = [y | y <- [1..x], x `mod` y == 0]

isPrime :: Integer -> Bool 
isPrime x = divisors x == [1,x]

allPrimes :: Integer -> [Integer]
-- allPrimes x will return all primes <= x
allPrimes x = [p | p <- [1..x], isPrime p]








