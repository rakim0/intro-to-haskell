power :: Integer -> Integer -> Integer
power n m = if m == 0 then 1 else n * power n (m - 1)

fastPower :: Integer -> Integer -> Integer
fastPower n 0 = 1
fastPower n m
  | even m = let y = fastPower n (m `div` 2) in y * y
  | odd m = let y = fastPower n (m `div` 2) in y * y * n

fibonacci :: Integer -> Integer
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)