merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) 
 | x <= y    = x : merge xs (y:ys) -- Made stable with <=
 | otherwise = y : merge (x:xs) ys

singles :: [Int] -> [[Int]]
singles = map (:[])

mergePairs :: [[Int]] -> [[Int]] 
mergePairs  [] = []
mergePairs [xs] = [xs]
mergePairs (x:y:ys) = merge x y: mergePairs ys

mergeAll:: [[Int]] -> [Int]
mergeAll [] = []
mergeAll [l] = l
mergeAll xs = mergeAll (mergePairs xs) 

sort = mergeAll . singles