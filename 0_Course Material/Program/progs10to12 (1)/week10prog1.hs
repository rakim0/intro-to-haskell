import Queue ( Queue, empty, isEmpty, enqueue, dequeue )

data BTree a = Nil | Node (BTree a) a (BTree a) 

tree1, tree2 :: BTree Integer
tree1 = Node (Node Nil 2 Nil) 3 (Node Nil 5 Nil) 
tree2 = Node (Node Nil 4 Nil) 6 tree1

isNil :: BTree a -> Bool 
isNil Nil   = True 
isNil _     = False 

size :: BTree a -> Integer 
size Nil            = 0
size (Node tl _ tr) = 1 + size tl + size tr 

rightSkewTree :: [a] -> BTree a
rightSkewTree       = foldr (Node Nil) Nil 

leftSkewTree :: [a] -> BTree a 
leftSkewTree        = foldr (\v t -> Node t v Nil) Nil

reflect :: BTree a -> BTree a
reflect Nil             = Nil 
reflect (Node tl x tr)  = Node (reflect tr) x (reflect tl) 

height :: BTree a -> Integer 
height Nil              = 0 
height (Node tl _ tr)   = 1 + max (height tl) (height tr)

bflist :: BTree a -> [a]
bflist t    = go (empty +: t) where 
    (+:) :: Queue (BTree a) -> BTree a -> Queue (BTree a)
    q +: t  = if isNil t then q else enqueue t q
    go :: Queue (BTree a) -> [a]
    go q    = if isEmpty q then [] else x: go (q' +: tl +: tr) where 
        (Node tl x tr, q')  = dequeue q

dflist :: BTree a -> [a]
dflist t                = go t [] where 
    go Nil l            = l 
    go (Node tl x tr) l = x: go tl (go tr l)

inorder :: BTree a -> [a]
-- inorder Nil             = []
-- inorder (Node tl x tr)  = inorder tl ++ [x] ++ inorder tr
inorder t               = go t [] where 
    go Nil l            = l
    go (Node tl x tr) l = go tl (x: go tr l) 

createTree :: [a] -> BTree a 
-- createTree []       = Nil 
-- createTree l        = Node (createTree front) x (createTree back) where 
--     (front, x:back) = splitAt (length l `div` 2) l 
createTree l        = fst $ go (length l) l where 
    go 0 xs         = (Nil, xs)
    go n xs         = (Node tl y tr, zs) where 
        m           = n `div` 2 
        (tl, y:ys)  = go m xs 
        (tr, zs)    = go (n-m-1) ys

instance Show a => Show (BTree a) where 
    show :: Show a => BTree a -> String
    show                        = concat . go 0 where 
        go n Nil                = [indent n "[" ++ "*]"]
        go n (Node Nil x Nil)   = [indent n "[" ++ show x ++ "]"]
        go n (Node tl x tr)     = [indent n "[" ++ show x ++ "\n"] ++ 
                                    go (n+2) tl ++ ["\n"] ++ 
                                    go (n+2) tr ++ ["\n"] ++ 
                                  [indent n "]"] 
        indent n s              = replicate n ' ' ++ s 


