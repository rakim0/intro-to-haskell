fib :: Int -> Integer 
fib n   = fst $ iterate (\(a,b) -> (b,a+b)) (0,1) !! n

lcs :: Eq a => [a] -> [a] -> [a]
lcs as bs       = t where 
    (_,t):_     = foldr nxtRow initRow as
    initRow     = replicate (length bs + 1) (0,[])
    nxtRow x    = go bs where 
        go [] _             = [(0,[])]
        go (y:ys) (u:v:vs)  = let t:ts = go ys (v:vs) in
            (if x == y then (1+fst v,x:snd v) 
                    else longer t u):t:ts
    longer t u  = if fst t >= fst u then t else u 

lcsNaive :: String -> String -> String
lcsNaive xs ys                  = snd (go xs ys) where 
    go "" _                     = (0, "") 
    go _  ""                    = (0, "")
    go xs@(x:xs') ys@(y:ys')    = upd x y (go xs ys') (go xs' ys) (go xs' ys') 
    upd x y mt nu (p,v) 
        | x == y                = (1+p,x:v)
        | fst mt >= fst nu      = mt
        | otherwise             = nu 

tails :: [a] -> [[a]]
tails []        = [[]]
tails (x:xs)    = (x:xs):tails xs 

longest :: [[a]] -> [a]
longest [l]     = l 
longest (l:ls)  = let l' = longest ls in 
    if length l >= length l' then l else l'

ascend :: Ord a => a -> [a] -> Bool
ascend _ []     = True 
ascend x (y:_)  = x < y

lasNaive :: Ord a => [a] -> [a]
lasNaive as     = longest (map go (tails as)) where 
    go []       = []
    go (x:xs)   = x:longest [l | l <- map go (tails xs), ascend x l] 

{-
gos xs = map go (tails xs)
gos [] = map go (tails []) = map go [[]] = [go []] = [[]]
gos (x:xs) = map go (tails (x:xs)) = go (x:xs): map go (tails xs) = go (x:xs): gos xs 
    = (x: longest [l | l <- gos xs, ascend x l]): gos xs
    = let ls = gos xs in 
        (x: longest [l | l <- ls, ascend x l]): ls
-}

las :: Ord a => [a] -> [a]
las as          = longest (gos as) where 
    gos []      = [[]]
    gos (x:xs)  = let ls = gos xs in (x:longest [l | l <- ls, ascend x l]): ls