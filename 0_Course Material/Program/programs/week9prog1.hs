------------------ Graph connectivity -----------------

type Vertex = Char
type Edge = (Vertex, Vertex)
vertices :: [Vertex]
edges :: [Edge]
vertices = "abcdef" 
edges = [('a','b'), ('a','d'), ('b','c'), ('f','d'),
        ('c','a'), ('c','e'), ('d','e'), ('f','e')]
                    
edge :: Vertex -> Vertex -> Bool
edge u v = (u,v) `elem` edges

type Path = [Vertex]

-- connected :: Vertex -> Vertex -> Bool 
-- connected u v = edge u v || not (null [w | w <- vertices, connected u w, edge w v])                  

extendPath :: Path -> [Path]
extendPath []       = [ [v] | v <- vertices]
extendPath (v:vs)   = [u:v:vs | u <- vertices, edge u v]

extendAll :: [Path] -> [Path]
extendAll = concatMap extendPath

emptyPath :: Path 
emptyPath = []

allPaths :: [[Path]]
allPaths = iterate extendAll [emptyPath]

shortPaths :: [Path]
shortPaths = concat $ take (length vertices + 1) allPaths 

connectedPairs :: [(Vertex, Vertex)]
connectedPairs = [(u, v) | u:p <- shortPaths, v <- u:p] 

connected :: Vertex -> Vertex -> Bool
connected u v = (u,v) `elem` connectedPairs 


------------- n queens ----------------------

type Config = [Int]
extendByOne         :: Int -> Config -> [Config]
extendByOne n conf  = [c:conf | c <- [0..(n-1)], safe c conf] where 
    safe c conf     =   c `notElem` [ j     | (i,j) <- occupiedCells]
                     && c `notElem` [ j-i   | (i,j) <- occupiedCells]
                     && c `notElem` [ j+i   | (i,j) <- occupiedCells]
    occupiedCells   = zip [1..] conf

extend :: Int -> Config -> [Config]
extend n conf = if length conf == n 
                then [conf] 
                else concatMap (extend n) (extendByOne n conf)

queens :: Int -> [Config]
queens n = extend n []

queens' :: Int -> [[Int]]
queens' n = extend [0] where 
    extend (l:conf) = if l == n then [conf] else 
        concatMap extend [l+1:c:conf | c <- [0..n-1], safe c] where 
        safe c = c `notElem` concat [[j, j-i, j+i] | (i,j) <- zip [1..] conf] 

queensone :: Int -> Config
queensone i = c where 
    c:_     = queens i