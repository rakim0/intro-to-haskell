module Heap
  ( Heap,
    emptyHeap,
    isEmpty,
    heapUnion,
    insertInHeap,
    findMax,
    deleteMax,
    createHeap,
    sortHeap,
    heapSort,
  )
where

data Heap a = Nil | Node Int a (Heap a) (Heap a)

emptyHeap :: Heap a
emptyHeap = Nil

isEmpty :: Heap a -> Bool
isEmpty Nil = True
isEmpty _ = False

size :: Heap a -> Int
size Nil = 0
size (Node s _ _ _) = s

root :: Heap a -> a
root (Node _ x _ _) = x

isHeap :: (Ord a) => Heap a -> Bool
isHeap Nil = True
isHeap (Node s x hl hr) =
  s == 1 + sl + sr
    && sl >= sr
    && (isEmpty hl || x >= root hl)
    && (isEmpty hr || x >= root hr)
    && isHeap hl
    && isHeap hr
  where
    (sl, sr) = (size hl, size hr)

realign :: Heap a -> Heap a
realign Nil = Nil
realign h@(Node s x hl hr) = if size hl >= size hr then h else Node s x hr hl

heapUnion :: (Ord a) => Heap a -> Heap a -> Heap a
heapUnion h1 Nil = h1
heapUnion Nil h2 = h2
heapUnion h1@(Node s1 x h1l h1r) h2@(Node s2 y h2l h2r)
  | x >= y = realign (Node (s1 + s2) x h1l (heapUnion h1r h2))
  | otherwise = realign (Node (s1 + s2) y h2l (heapUnion h1 h2r))

insertInHeap :: (Ord a) => a -> Heap a -> Heap a
insertInHeap x h = heapUnion (Node 1 x Nil Nil) h

findMax :: Heap a -> Maybe a
findMax h = if isEmpty h then Nothing else Just $ root h

deleteMax :: (Ord a) => Heap a -> (Maybe a, Heap a)
deleteMax Nil = (Nothing, Nil)
deleteMax (Node _ x hl hr) = (Just x, heapUnion hl hr)

leftistTree :: (Ord a) => [a] -> Heap a
leftistTree xs = fst (go (length xs) xs)
  where
    go 0 xs = (Nil, xs)
    go n xs = (Node n y hl hr, zs)
      where
        m = n `div` 2
        (hl, y : ys) = go m xs
        (hr, zs) = go (n - m - 1) ys

data Badness = NoBad | LeftBad | RightBad

badness :: (Ord a) => Heap a -> Badness
badness (Node _ x hl hr)
  | x >= max y z = NoBad
  | y >= z = LeftBad
  | otherwise = RightBad
  where
    y = if isEmpty hl then x else root hl
    z = if isEmpty hr then x else root hr

xchngLeft :: Heap a -> Heap a
xchngLeft (Node s x (Node sl y hll hlr) hr) =
  Node s y (Node sl x hll hlr) hr

xchngRight :: Heap a -> Heap a
xchngRight (Node s x hl (Node sr y hrl hrr)) =
  Node s y hl (Node sr x hrl hrr)

sift :: (Ord a) => Heap a -> Heap a
sift Nil = Nil
sift h = case badness h of
  NoBad -> h
  LeftBad -> Node s x (sift hl) hr
    where
      Node s x hl hr = xchngLeft h
  RightBad -> Node s x hl (sift hr)
    where
      Node s x hl hr = xchngRight h

heapify :: (Ord a) => Heap a -> Heap a
heapify Nil = Nil
heapify (Node s x hl hr) = sift (Node s x (heapify hl) (heapify hr))

createHeap :: (Ord a) => [a] -> Heap a
createHeap = heapify . leftistTree

sortHeap :: (Ord a) => Heap a -> [a]
sortHeap = reverse . go
  where
    go Nil = []
    go (Node s x hl hr) = x : go (heapUnion hl hr)

heapSort :: (Ord a) => [a] -> [a]
heapSort = sortHeap . createHeap

{-
instance (Show a) => Show (Heap a) where
  show = draw

draw :: (Show a) => Heap a -> String
draw t = foldr (\s1 s2 -> s1 ++ "\n" ++ s2) "" lines
  where
    (lines, m, n) = go t

go :: (Show a) => Heap a -> ([String], Int, Int)
go Nil = (["*"], 0, 0)
go (Node _ x Nil Nil) = ([sx], p, q)
  where
    (sx, lenx, p, q) = (show x, length sx, lenx `div` 2, lenx - p - 1)
go (Node _ x tl tr) = (line1 : line2 : rest, m, n)
  where
    (sx, lenx, p, q) = (show x, length sx, lenx `div` 2, lenx - p - 1)
    (linesl, ml, nl) = go tl
    (linesr, mr, nr) = go tr
    -- (arml,padl)         = if nl >= p+2 then (nl-p-1,0) else (1,p+2-nl)
    -- (armr,padr)         = if mr >= q+2 then (mr-q-1,0) else (1,q+2-mr)
    (arml, padl) = if nl >= p + 2 then (nl - p, 1) else (2, p + 3 - nl)
    (armr, padr) = if mr >= q + 2 then (mr - q, 1) else (2, q + 3 - mr)
    (m, n) = (ml + arml + p + 2, armr + nr + q + 2)
    line1 =
      replicate ml ' '
        ++ "+"
        ++ replicate arml '-'
        ++ " "
        ++ sx
        ++ " "
        ++ replicate armr '-'
        ++ "+"
        ++ replicate nr ' '
    line2 =
      replicate ml ' '
        ++ "|"
        ++ replicate (arml + armr + lenx + 2) ' '
        ++ "|"
        ++ replicate nr ' '
    rest = join linesl linesr
    join xss yss
      | null xss = map (replicate (ml + nl + padl + padr + 2) ' ' ++) yss
      | null yss = map (++ replicate (padl + padr + mr + nr + 2) ' ') xss
      | otherwise =
          (xs ++ replicate (padl + padr + 1) ' ' ++ ys)
            : join xss' yss'
      where
        (xs : xss') = xss
        (ys : yss') = yss
-}
