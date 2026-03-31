-- lists
addAtEnd :: Int -> [Int] -> [Int]
addAtEnd x [] = [x]
addAtEnd x (start : rest) = start : addAtEnd x rest

attach :: [Int] -> [Int] -> [Int]
attach l1 ys = foldl (flip addAtEnd) l1 ys

attach2 :: [a] -> [a] -> [a]
attach2 l1 l2 = l1 ++ l2

h :: (Integral b) => b -> b -> (b, (b, b))
h a b = go (a, b) (1, 0) (0, 1)
  where
    go (r', r) (s', s) (t', t) =
      if r == 0
        then (r', (s', t'))
        else
          let q = r' `div` r in go (r, r' - q * r) (s, s' - q * s) (t, t' - q * t)

balloons str = go str []
  where
    go "" _ = 0
    go (x : xs) s = if x `elem` s then 1 + go xs s else 2 + go xs (x : s)

valueAtPosition n _
  | n < 0 = error "Negative index"
valueAtPosition 0 [] = error "Value too large"
valueAtPosition n l = l !! n

reverseList :: [Int] -> [Int]
-- helper [] l2 = l2
-- helper (x:xs) l2 = helper xs (x:l2)
-- reverseList l1 = helper l1 []
reverseList [] = []
reverseList (x : xs) = reverseList xs ++ [x]

myGcd :: Int -> Int -> Int
myGcd a b
  | a < 0 || b < 0 = myGcd (abs a) (abs b)
  | b == 0 = a
  | otherwise = myGcd b (a `mod` b)

myGcdTrace :: (Int, Int) -> [(Int, Int)]
myGcdTrace (a, b)
  | a < 0 || b < 0 = (a, b) : myGcdTrace (abs a, abs b)
  | a < b = (a, b) : myGcdTrace (b, a)
  | b == 0 = [(a, b)]
  | otherwise = (a, b) : myGcdTrace (b, a `mod` b)

fib' 0 = (0, 1)
fib' 1 = (1, 1)
fib' n = (y, x + y)
  where
    (x, y) = fib' (n - 1)

addOne [] = []
addOne (x : xs) = x + 1 : addOne xs

(#) :: [Int] -> Int -> [Int]
[] # x = [x]
(x : xs) # y = x : (xs # y)
