type Config = [Int]

extendOne :: Int -> Config -> [Config]
extendOne n conf = [c:conf | c <- [0..(n-1)], good c] 
    where 
        good c = c `notElem` concat [[j,j-i,j+i] | (i,j) <- occupiedCells]
        occupiedCells = zip  [1..] conf

extendConf :: Int -> Config -> [Config]
extendConf n conf = 
    if length conf == n then [conf]
    else concatMap (extendConf n) (extendOne n conf)

getQueens n = extendConf n []

getOneQueen n = let confs = getQueens n in 
                if length confs > 0 then print( confs !! 0)
                else 
                    print ("not possible")