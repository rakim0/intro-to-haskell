data BSTDict a b
  = Nil
  | Node (BSTDict a b) (a, b) (BSTDict a b)
  deriving (Show)

isEmpty :: BSTDict a b -> Bool
isEmpty Nil = True
isEmpty _ = False

inorder :: BSTDict a b -> [(a, b)]
inorder Nil = []
inorder (Node l kv r) =
  inorder l ++ [kv] ++ inorder r

query :: (Ord a) => a -> BSTDict a b -> Maybe b
query _ Nil = Nothing
query k (Node l (k', v) r)
  | k == k' = Just v
  | k < k' = query k l
  | otherwise = query k r

insert :: (Ord a) => (a, b) -> BSTDict a b -> BSTDict a b
insert (k, v) Nil =
  Node Nil (k, v) Nil
insert (k, v) (Node l (k', v') r)
  | k == k' = Node l (k, v) r
  | k < k' = Node (insert (k, v) l) (k', v') r
  | otherwise = Node l (k', v') (insert (k, v) r)

deleteMax :: BSTDict a b -> ((a, b), BSTDict a b)
deleteMax (Node l kv Nil) =
  (kv, l)
deleteMax (Node l kv r) =
  (maxKV, Node l kv newRight)
  where
    (maxKV, newRight) = deleteMax r

delete :: (Ord a) => a -> BSTDict a b -> BSTDict a b
delete _ Nil = Nil
delete k (Node l (k', v') r)
  | k < k' =
      Node (delete k l) (k', v') r
  | k > k' =
      Node l (k', v') (delete k r)
  | otherwise =
      case (l, r) of
        (Nil, Nil) -> Nil
        (Nil, _) -> r
        (_, Nil) -> l
        _ ->
          let (maxKV, newLeft) = deleteMax l
           in Node newLeft maxKV r

main :: IO ()
main = do
  bstActions Nil

bstActions :: BSTDict Int String -> IO ()
bstActions tree = do
  cmd <- getLine

  let l = words cmd

  case l of
    ["insert", tupleStr] -> do
      let (k, v) = read tupleStr :: (Int, String)

      let newTree = insert (k, v) tree

      putStrLn $
        unwords (map snd (inorder newTree))

      bstActions newTree
    ["delete", kStr] -> do
      let k = read kStr :: Int

      let newTree = delete k tree

      putStrLn $
        unwords (map snd (inorder newTree))

      bstActions newTree
    ["query", kStr] -> do
      let k = read kStr :: Int

      case query k tree of
        Nothing ->
          putStrLn $
            "No element with key "
              ++ show k
              ++ " in the tree!"
        Just v ->
          putStrLn $
            "Value of key "
              ++ show k
              ++ " is \""
              ++ v
              ++ "\""

      bstActions tree
    ["bye"] -> do
      putStrLn "Exiting! Good bye!"
    _ -> do
      putStrLn "Invalid operation!"
      bstActions tree