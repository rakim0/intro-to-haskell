merge :: [Int] -> [Int] -> [Int]
merge [] r = r
merge l [] = l
merge (x : xs) (y : ys)
  | x < y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

mySort :: [Int] -> [Int]
mySort [] = []
mySort [x] = [x]
mySort xs = merge (mySort front) (mySort back)
  where
    m = (length xs) `div` 2
    (front, back) = splitAt m xs