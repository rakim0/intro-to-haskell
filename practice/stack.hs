module Stack(push, pop, isEmpty, empty) where

data Stack a = [a] 
newtype Stack a = Stack [a] deriving Eq
instance Show a => Show (stack a) where
    show (Stack l) = fancyShow l

fancyShow :: Show a => [a] -> String
fancyShow = (intercalate " -> ") . map show 

push x (Stack xs) = (Stack x:xs)
isEmpty (Stack x) = x == []
empty = Stack []
pop (Stack []) = error "Empty Stack"
pop (Stack (x:xs)) = (x, Stack(xs)) 