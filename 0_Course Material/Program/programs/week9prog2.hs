newtype Stack a = Stack [a]

push :: a -> Stack a -> Stack a
push x (Stack xs)   = Stack (x:xs) 

pop :: Stack a -> (a, Stack a)
pop (Stack [])      = error "Empty stack"
pop (Stack (x:xs))  = (x, Stack xs)

empty :: Stack a 
empty = Stack []

isEmpty :: Stack a -> Bool
isEmpty (Stack xs)  = null xs 

sumStack :: Num a => Stack a -> a
sumStack (Stack xs) = sum xs

data Exp = Val Int | Add Exp Exp | Sub Exp Exp | Mul Exp Exp | Div Exp Exp 
    deriving Eq

instance Show Exp where 
    show :: Exp -> String
    show (Val n) = show n
    show (Add expl expr) = "(" ++ show expl ++ " + " ++ show expr ++ ")"
    show (Sub expl expr) = "(" ++ show expl ++ " - " ++ show expr ++ ")"
    show (Mul expl expr) = "(" ++ show expl ++ " * " ++ show expr ++ ")"
    show (Div expl expr) = "(" ++ show expl ++ " / " ++ show expr ++ ")"

exp1, exp2, exp3 :: Exp 
exp1 = Add (Val 3) (Mul (Val 5) (Val 2))
exp2 = Mul (Add (Val 3) (Val 5)) (Val 2)
exp3 = Mul (Div (Val 22) (Val 3)) (Add (Val 4) (Val 5))

eval :: Exp -> Int 
eval (Val n)        = n 
eval (Add e1 e2)    = eval e1 + eval e2
eval (Sub e1 e2)    = eval e1 - eval e2
eval (Mul e1 e2)    = eval e1 * eval e2
eval (Div e1 e2)    = eval e1 `div` eval e2

type Token = Either Int (Exp -> Exp -> Exp) 
strToTokList :: String -> [Token] 
strToTokList str = map tokenize (words str) where 
    tokenize :: String -> Token 
    tokenize "+" = Right Add 
    tokenize "-" = Right Sub 
    tokenize "*" = Right Mul 
    tokenize "/" = Right Div 
    tokenize str = Left (read str :: Int)  

tokListToExp :: [Token] -> Exp 
tokListToExp toklist    = fst (pop st) where 
    st                  = foldl step empty toklist
    step :: Stack Exp -> Token -> Stack Exp
    step st (Left n)    = push (Val n) st 
    step st (Right op)  = push (op exp2 exp1) st2 where 
        (exp1, st1)     = pop st
        (exp2, st2)     = pop st1

strToExp :: String -> Exp 
strToExp    = tokListToExp . strToTokList

evalExpStr :: String -> Int 
evalExpStr  = eval . strToExp 
