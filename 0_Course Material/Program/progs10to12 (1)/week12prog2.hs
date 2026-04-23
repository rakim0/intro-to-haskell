import Control.Monad ( replicateM )
import System.Random ( randomRIO )
import Data.Array ( (!), listArray ) 
lcs :: String -> String -> (Int, String) 
lcs as bs                   = lcsArr!(0,0) where 
        (m,n)                   = (length as, length bs)
        aArr                    = listArray (0, m) as 
        bArr                    = listArray (0, n) bs  
        lcsArr                  = listArray ((0,0), (m,n)) 
            [lcsMemo (i,j) | i <- [0..m], j <- [0..n]]
        lcsMemo (i,j)
            | i == m || j == n  = (0,"")
            | aArr!i == bArr!j  = (1+l,aArr!i:s) 
            | l1 >= l2          = (l1,s1) 
            | otherwise         = (l2,s2) where  
                (l,s)           = lcsArr!(i+1,j+1)
                (l1,s1)         = lcsArr!(i,j+1)
                (l2,s2)         = lcsArr!(i+1,j)

las :: [Int] -> [Int]
las as              = longest [lasArr!i | i <- [0..n]] where 
    n               = length as 
    aArr            = listArray (0,n) as
    lasArr          = listArray (0,n)
        [lasMemo i | i <- [0..n]]
    lasMemo i 
        | i == n    = [] 
        | otherwise = (aArr!i):longest [lasArr!j | j <- [i+1..n], 
            ascend (aArr!i) (lasArr!j)]
    ascend x []     = True 
    ascend x (y:_) = x < y  
    longest = foldr (\l acc -> if length l >= length acc then l else acc) []
randIntList :: Int -> IO [Int]
randIntList sz  = replicateM sz (randomRIO (-50,100)) 

randString :: Int -> IO String 
randString sz   = replicateM sz (randomRIO ('a','z'))

fib :: Integer -> Integer
fib n = fibArr!n where 
    fibArr  = listArray (0,n) [f i | i <- [0..n]]
    f 0     = 0
    f 1     = 1 
    f i     = fibArr!(i-2) + fibArr!(i-1)