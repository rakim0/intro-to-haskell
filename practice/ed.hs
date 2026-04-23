import Data.Array((!), listArray)
{-
    edit distance
    you can insert
    you can delete
    you can change
-}

ed as bs = edArr!(0,0) where
    (n,m) = (length as, length bs)
    aArr = listArray (0,n) as
    bArr = listArray (0,m) bs
    edArr = listArray ((0,0), (n,m)) [edMemo(i,j) | i <- [0..n], j <- [0..m]]
    edMemo (i, j) 
        | i == n || j == m = (n - i) + (m - j)
        | aArr!i == bArr!j = skipab
        | otherwise = 1 + minimum[skipa,skipb,skipab]
            where
                skipa = edArr!(i+1, j)
                skipb = edArr!(i, j+1)
                skipab = edArr!(i+1, j+1)
