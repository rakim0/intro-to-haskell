fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fib n = fst $ iterate(\(a,b) -> (b, a+b)) (0, 1) !! n

{-
    also learn the arrays version of fibonacci
-}

fib n = fibArr!n where
    fibArr = listArray (0,n) [f i | i <- [0..n]]
    f 0 = 0
    f 1 = 1
    f n = fibArr!(n-1)+fibArr!(n-2)