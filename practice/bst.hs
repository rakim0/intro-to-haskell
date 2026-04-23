{-

    Write a BST implementation that does the following
    
    inorder
    query
    insert 
    delete

-}

data BST a = Nil | Node (BST a) a (BST a)

inorder :: BST a -> [a]
inorder Nil = []
inorder (Node l root r) = (inorder l)++[root]++(inorder r)

query :: Ord(a) => a -> BST a -> Bool
query a Nil = False
query a (Node l root r) 
    | (a == root) = True
    | (a < root) = query a l
    | (a > root) = query a r

insert :: Ord(a) => a -> BST a -> BST a
insert a Nil = (Node Nil a Nil)
insert a (Node l root r) 
    | (a == root) = (Node l root r)
    | (a < root) = Node (insert a l) root r 
    | (a > root) = Node l root (insert a r)

-- gets the rightmost node in the tree and deletes it from the end
deleteMax :: BST a -> (a, BST a)
deleteMax (Node ltree root Nil) = (root, ltree)
deleteMax (Node ltree root rtree) = (mxValue, Node ltree root newRight) 
                                    where (mxValue, newRight) = deleteMax rtree

delete :: Ord(a) => a -> BST a -> BST a
delete a Nil = Nil
delete a (Node l root r) 
    | a < root = Node (delete a l) root r
    | a > root = Node l root (delete a r)
    | otherwise = case (l, r) of
        (Nil, Nil) -> Nil
        (Nil, _) -> r
        (_, Nil) -> l
        _ -> (Node newL maxValue r) where (maxValue, newL) = (deleteMax l)