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

solve           :: Grid -> [Grid]
solve           = filter valid . expand . choices

solution        :: Grid -> Grid
solution        = (!!0) . solve 

-- This solution is actually useless, since even with half the grid filled, we have 
-- 9^40 = 147808829414345923316083210206383297601
-- grids to check

    
