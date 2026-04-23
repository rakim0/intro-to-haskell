import Data.Char 

myToUpper :: Char -> Char 
myToUpper c     = if isLower c then chr (ord c - 32) else c 

myTU1 :: Char -> Char 
myTU1 c 
    | isLower c = chr (ord c + offset)
    | otherwise = c where
        offset  = ord 'A' - ord 'a'

myTU2 :: Char -> Char 
myTU2 c 
    | isLower c = let 
        offset  = ord 'A' - ord 'a' in 
                    chr (ord c + offset)
    | otherwise = c

occurs :: Char -> String -> Bool 
occurs c []     = False 
occurs c (d:ds) = c == d || occurs c ds

capitalize :: String -> String
capitalize ""       = ""
capitalize (c:cs)   = toUpper c: capitalize cs

position :: Char -> String -> Int 
position c ""       = 0 
position c (d:ds)   = if c == d then 0 else 1 + position c ds

wordc :: String -> Int 
wordc s = go (' ':s) where 
    go [c]      = 0
    go (c:d:ds) = if isSpace c && not (isSpace d) then 
                    1 + go (d:ds) else 
                        go (d:ds)