type Vertex = Char
type Edge = (Vertex, Vertex)


vertices :: [Vertex]
vertices = "abcdef"

edges :: [Edge]
edges = [('a','b'), ('a','d'), ('b','c'), ('f', 'd'), ('c','a'), ('c','e'), ('d','e'), ('f','e')]

edge :: Vertex -> Vertex -> Bool

edge u v = (u,v) `elem` edges

{-
to check if connected is true or not:
    build a list of infinite paths
-}

type Path = [Vertex]

extendPath :: Path -> [Path]
extendPath [] = [[v] | v <- vertices]
extendPath (v:vs) = [u:v:vs | u <- vertices, edge u v]

extendAll :: [Path] -> [Path]
extendAll = concatMap extendPath

allPaths :: [[Path]]
allPaths = iterate extendAll [[]]

shortPaths = (take (length vertices+1) allPaths)
connectedPairs = [(u,v)| u:p <- shortPaths, v <- u:p]
connected u v = (u, v) `elem` connectedPairs










