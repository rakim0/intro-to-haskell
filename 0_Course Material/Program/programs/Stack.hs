module Stack(Stack, push, pop, top, fromList, empty, isEmpty) where 
import Data.List ( intercalate ) 

newtype Stack a = Stack [a] 
    deriving Eq 

instance Show a => Show (Stack a) where 
    show :: Show a => Stack a -> String
    show (Stack xs) = intercalate " |-> " (map show xs)

push :: a -> Stack a -> Stack a
push x (Stack xs)   = Stack (x:xs)

pop :: Stack a -> (a, Stack a)
pop (Stack [])      = error "Empty stack" 
pop (Stack (x:xs))  = (x, Stack xs)

top :: Stack a -> a
top (Stack [])      = error "Empty stack"
top (Stack (x:_))  = x

fromList :: [a] -> Stack a 
fromList    = Stack 

empty :: Stack a 
empty = Stack [] 

isEmpty :: Stack a -> Bool 
isEmpty (Stack xs)  = null xs 
