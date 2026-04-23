import Control.Monad ( replicateM )
import System.Random ( randomRIO )
import Data.List ( partition )

randInts :: Int -> (Int, Int) -> IO [Int]
randInts sz (l,u) = replicateM sz (randomRIO (l,u))

insert :: Ord a => a -> [a] -> [a]
insert v []         = [v]
insert v l@(x:xs)   = if v <= x then v:l else x:insert v xs

isort :: Ord a => [a] -> [a] 
isort = foldr insert []

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys         = ys 
merge xs []         = xs 
merge (x:xs) (y:ys) = if x <= y
    then x:merge xs (y:ys)
    else y:merge (x:xs) ys 

msort :: Ord a => [a] -> [a]
msort []            = []
msort [x]           = [x]
msort l             = merge (msort front) (msort back) where 
    n               = length l
    (front, back)   = splitAt (n `div` 2) l

qsort :: Ord a => [a] -> [a]
qsort []            = []
qsort [x]           = [x]
qsort (x:xs)        = qsort front ++ [x] ++ qsort back where
    (front, back)   = partition (<= x) xs 

isSorted :: Ord a => [a] -> Bool 
isSorted []         = True 
isSorted [_]        = True 
isSorted (x:y:ys)   = x <= y && isSorted (y:ys)
 
singletons :: [a] -> [[a]]
singletons          = map (:[])

mergePairs :: Ord a => [[a]] -> [[a]]
mergePairs []           = []
mergePairs [l]          = [l]
mergePairs (l1:l2:ls)   = merge l1 l2: mergePairs ls 

mergeAll :: Ord a => [[a]] -> [a]
mergeAll []         = []
mergeAll [l]        = l 
mergeAll xs         = mergeAll (mergePairs xs)

msort' :: Ord a => [a] -> [a]
msort'              = mergeAll . singletons

