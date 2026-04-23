import Control.Monad ( replicateM )
import System.Random ( randomRIO )
fastRev :: [a] -> [a]
fastRev             = myFoldl (flip (:)) []

myFoldl :: (t1 -> t2 -> t1) -> t1 -> [t2] -> t1
myFoldl f v []      = v
myFoldl f v (x:xs)  = myFoldl f (f v x) xs

insert :: Ord a => a -> [a] -> [a]
insert v []         = [v]
insert v l@(x:xs)   = if v <= x then v:l else x:insert v xs

isort :: Ord a => [a] -> [a] 
isort = foldr insert []

randInts :: Int -> (Int, Int) -> IO [Int]
randInts sz (l,u) = replicateM sz (randomRIO (l,u))

