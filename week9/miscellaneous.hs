{-
    Implement map using foldr.

    Write a function isPalindrome :: String -> Bool using recursion.

    Write a function squish :: [[a]] -> [a] that flattens a list of lists.

    Define a data type Tree a for a Binary Search Tree and write the insert function.

    Write an IO action that reads integers from the user until a 0 is entered, then prints the sum.
-}

myMap :: (a->b) -> [a] -> [b]
myMap f = foldr (\x acc -> (f x):acc) []


isPalindrome :: String -> Bool
isPalindrome [] = True
isPalindrome [s1] = True
isPalindrome str = (head str == last str) && (isPalindrome (tail $ init str))

main :: IO()
main = do
    total <- y
    print total

y = do 
    x <- readLn :: IO Int
    if x == 0 then return 0
    else do
        z <- y
        return (x+z)

Data Tree a = Nil | Node (Tree a) a (Tree a) 

insertInTree :: (Ord a) => a -> Tree a -> Tree a
insertInTree x Nil = (Node Nil x Nil)
insertInTree x (Node l root r) 
    | (x < root) = (Node (insertInTree x l) root r)
    | (x > root) = (Node l root (insertInTree x r))
    | otherwise = (Node l root r)













