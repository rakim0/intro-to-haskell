data BST a = Nil | Node (BST a) a (BST a)

inorder :: BST a -> [a]
inorder Nil = []
inorder (Node ltree a rtree) = (inorder ltree) ++ [a] ++ (inorder rtree)

query :: (Ord a) => a -> BST a -> Bool
query x Nil = False
query x (Node ltree a rtree)
  | (x == a) = True
  | (x < a) = (query x ltree)
  | (x > a) = (query x rtree)

insert :: (Ord a) => a -> BST a -> BST a
insert x Nil = Node Nil x Nil
insert x (Node ltree root rtree)
  | x < root = Node (insert x ltree) root rtree
  | x > root = Node ltree root (insert x rtree)
  | otherwise = Node ltree root rtree

deleteMax :: (Ord a) => BST a -> (a, BST a)
deleteMax (Node ltree root Nil) = (root, ltree)
deleteMax (Node l root r) = (mxValue, Node l root newRight)
  where
    (mxValue, newRight) = deleteMax r

delete :: (Ord a) => a -> BST a -> BST a
delete a Nil = Nil
delete a (Node l root r)
  | a < root = (Node (delete a l) root r)
  | a > root = (Node l root (delete a r))
  | otherwise = case (l, r) of
      (Nil, Nil) -> Nil
      (Nil, _) -> r
      (_, Nil) -> l
      _ -> Node newLeft mxVal r where (mxVal, newLeft) = deleteMax l