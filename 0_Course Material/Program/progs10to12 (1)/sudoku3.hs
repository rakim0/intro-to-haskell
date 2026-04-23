import Data.List ( intercalate, intersperse, (\\) )
import Control.Monad ( replicateM )
import System.IO ( openFile, IOMode(..), hGetLine, hClose )
import System.Environment ( getArgs )

type Matrix a   = [Row a]
type Row a      = [a]
type Grid       = Matrix Digit
type Digit      = Char

-- Input/output part

main :: IO ()
main = do 
    (fn:_) <- getArgs 
    inh <- openFile fn ReadMode
    grid <- replicateM 9 (hGetLine inh)  
    putStrLn "Your input is ..."
    putStrLn $ showGrid grid
    let sols = solve grid in 
        if null sols
        then putStrLn "This puzzle has no solution" 
        else do 
            putStrLn "Your solution is ... "
            let s:_ = sols in putStrLn $ showGrid s
    hClose inh 

solveAndShow :: Grid -> IO ()
solveAndShow grid = do
    putStrLn "Your input is ..."
    putStrLn $ showGrid grid
    let sols = solve grid 
    if null sols 
    then putStrLn "This puzzle has no solution" 
    else do
        putStrLn "Your solution is ... "
        let s:_ = sols in putStrLn $ showGrid s

showGrid        :: Grid -> String
showGrid g      = intercalate "\n" (map showRow g) where 
    showRow row = replicate 24 ' ' ++ intersperse ' ' row
                            
-- The actual program is here

digits          :: [Digit]
digits          = "123456789"

blank           :: Digit -> Bool
blank           = (== '-')

type Choices    = [Digit]

choices         :: Grid -> Matrix Choices
choices         = map (map choice)

choice          :: Digit -> [Digit]
choice d        = if blank d then digits else [d]

expand          :: Matrix Choices -> [Grid]
expand          = cp . map cp

cp              :: [[a]] -> [[a]]
cp []           = [[]]
cp (xs:xss)     = [x:ys | x <- xs, ys <- cp xss]

valid           :: Grid -> Bool
valid g         = all nodups (rows g) && all nodups (cols g) 
                                    && all nodups (boxs g)

nodups          :: Eq a => [a] -> Bool
nodups []       = True
nodups (x:xs)   = x `notElem` xs && nodups xs

rows            :: Matrix a -> Matrix a
rows            = id

cols            :: Matrix a -> Matrix a
cols [xs]       = [[x] | x <- xs]
cols (xs:xss)   = zipWith (:) xs (cols xss)

boxs            :: Matrix a -> Matrix a
boxs            = map ungroup . ungroup . map cols . group . map group

group           :: [a] -> [[a]]
group []        = []
group xs        = take 3 xs:group (drop 3 xs)

ungroup         :: [[a]] -> [a]
ungroup         = concat

-- This solution is actually useless, since even with half the grid filled, we have 
-- 9^40 = 147808829414345923316083210206383297601
-- grids to check

-- We want a prune such that filter valid . expand = filter valid . expand . prune

pruneRow        :: Row Choices -> Row Choices
pruneRow row    = map (remove fixed) row where
    fixed       = [d | [d] <- row]
    
remove          :: Eq a => [a] -> [a] -> [a]
remove xs ds    = if singleton ds then ds else ds \\ xs

singleton       :: [a] -> Bool
singleton l     = length l == 1

pruneBy         :: (Matrix Choices -> Matrix Choices) -> Matrix Choices -> Matrix Choices
pruneBy f       = f . map pruneRow . f

prune           :: Matrix Choices -> Matrix Choices
prune           = pruneBy boxs . pruneBy cols . pruneBy rows

{-
Recall the following equation
      filter valid . expand
    = filter valid . expand . prune

Next idea:
    Instead of expanding all cells in the matrix of choices, 
    expand only one cell at a time. A good choice is the 
    smallest non-singleton cell
-}

expand1         :: Matrix Choices -> [Matrix Choices]
expand1 rows    = [rows1 ++ [row1 ++ [c]:row2] ++ rows2 | c <- cs] where
    (rows1, row:rows2)  = break (any smallest) rows
    (row1, cs:row2)     = break smallest row
    smallest cs         = length cs == n
    n                   = minimum (counts rows)
    counts              = filter (/=1) . map length . concat    

complete        :: Matrix Choices -> Bool
complete        = all (all singleton)

safe            :: Matrix Choices -> Bool
safe m          = all ok (rows m) && all ok (cols m) && all ok (boxs m) where
    ok row      = nodups [d | [d] <- row]

{-      
On incomplete matrices,
    expand = concatMap expand . expand1

Thus we have the following calculation:
      filter valid . expand
    = { by above property of expand    }
      filter valid . concatMap expand . expand1
    = { filter p . concat = concat . map (filter p)    }
      concat . map (filter valid . expand) . expand1
    = { filter valid . expand = filter valid . expand . prune    }
      concat . map (filter valid . expand . prune) . expand1

Letting search = filter valid . expand . prune, 
    we have on safe but incomplete matrices,
        search = concat . map search . expand1 . prune
-}

solve           :: Grid -> [Grid]
solve           = search . choices

search          :: Matrix Choices -> [Grid]
search m
    | not (safe m') = []
    | complete m'   = [map (map (!!0)) m']
    | otherwise     = concatMap search (expand1 m') where 
        m'          = prune m
        