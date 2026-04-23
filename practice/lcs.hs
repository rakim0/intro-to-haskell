import Data.Array ((!), listArray)

lcs a b = lcsArray!(0,0) where
    (m, n) = (length a, length b)
    aArr = listArray(0, m) a
    bArr = listArray(0, n) b
    lcsArray = listArray ((0,0), (m,n)) [lcsMemo (i,j) | i<-[0..m], j <- [0..n]]  
    lcsMemo(i,j)=
        let
            (l1, s1) = lcsArray!(i+1, j)
            (l2, s2) = lcsArray!(i, j+1)
            (l,s) = lcsArray!(i+1, j+1)
        in 
        if (i == m) || (j == n) then (0, "")
        else if (aArr!i) == (bArr!j) then (l+1, aArr!i:s)
        else if l1 >= l2 then (l1, s1)
        else  (l2, s2) 