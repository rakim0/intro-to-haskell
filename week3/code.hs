import Data.List

divisors x = [y | y <- [1 .. x], x `mod` y == 0]

primes x = [y | y <- [1 .. x], divisors y == [1, y]]

getPairs = [(x, y) | x <- [1 .. 10], y <- [1 .. 2], x ^ 2 + y ^ 2 < 100, y < x]

f :: [Int] -> Int -> [Int]
f [] _ = []
f (x : xs) y = if x == y then xs else x : (f xs y)

g :: [Int] -> [Int] -> [Int]
g [] _ = []
g x [] = x
g (x : xs) ys = if x `elem` ys then (g xs (f ys x)) else x : (g xs ys)

wordc :: [Char] -> Int
wordc "" = 1
wordc (ch : rest) = if ch == ' ' then 1 + wordc rest else wordc rest
