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

-- We want a prune such that filter valid . expand = filter valid . expand . prune

pruneRow        :: Row Choices -> Row Choices
pruneRow row    = map (remove fixed) row where
    fixed       = [d | [d] <- row]
    
remove          :: Eq a => [a] -> [a] -> [a]
remove xs ds    = if length ds == 1 then ds else ds \\ xs

pruneBy         :: (Matrix Choices -> Matrix Choices) -> Matrix Choices -> Matrix Choices
pruneBy f       = f . map pruneRow . f

prune           :: Matrix Choices -> Matrix Choices
prune           = pruneBy boxs . pruneBy cols . pruneBy rows

solve           :: Grid -> [Grid]
solve           = filter valid . expand . prune . choices

solution        :: Grid -> Grid
solution        = (!!0) . solve 
                    
{-
Note the following:
    rows . rows = id
    cols . cols = id
    boxs . boxs = id

    map rows . expand = expand . rows                   -- (1)
    map cols . expand = expand . cols                   -- (2)
    map boxs . expand = expand . boxs                   -- (3)

    filter nodups . cp = filter nodups . cp . pruneRow  -- (4)

    If f . f = id then
        filter (p . f) = map f . filter p . map f       -- (5)
    In other words, 
        filter (p . f) . map f = map f . filter p       - (5')

        filter (all p) . cp = cp . map (filter p)       -- (6)

    Now, 
        filter valid . expand =   
            filter (all nodups . boxs) .
            filter (all nodups . cols) .
            filter (all nodups . rows) . expand

    Now,
          filter (all nodups . boxs) . expand
        = {    by 5, since boxs . boxs = id    }
          map boxs . filter (all nodups) . map boxs . expand
        = {    by 3    }
          map boxs . filter (all nodups) . expand . boxs
        = {    def. of expand    }
          map boxs . filter (all nodups) . cp . map cp . boxs
        = {    6 and map f . map g = map (f . g)    }
          map boxs . cp . map (filter nodups . cp) . boxs
        = {    4     }
          map boxs . cp . map (filter nodups . cp . pruneRow) . boxs
        = { 6 and using map (f . g . h) = map p . map g . map h    }
          map boxs . filter (all nodups) . cp . map cp . map pruneRow . boxs
        = {    def. of expand    }
          map boxs . filter (all nodups) . expand . map pruneRow . boxs
        = { 5'    }
          filter (all nodups . boxs) . map boxs . expand . map pruneRow . boxs
        = { 3    }
          filter (all nodups . boxs) . expand . boxs . map pruneRow . boxs

    Thus
          filter (all nodups . boxs) . expand
        = filter (all nodups . boxs) . expand . pruneBy boxs

    We can repeat the same for rows and cols, and get
          filter valid . expand
        = filter valid . expand . prune
-}
