data Nat = Zero | Succ Nat deriving (Eq)

instance Show Nat where 
    show :: Nat -> String
    show = show . natToInt

isZero :: Nat -> Bool
isZero Zero = True 
isZero _ = False 

isZero' :: Nat -> Bool 
isZero' = (== Zero)

pred :: Nat -> Nat 
pred Zero       = Zero 
pred (Succ n)   = n 

even' :: Nat -> Bool
even' Zero = True 
even' (Succ Zero) = False
even' (Succ (Succ n)) = even' n

plus :: Nat -> Nat -> Nat
plus m Zero = m 
plus m (Succ n) = Succ (plus m n)

mult :: Nat -> Nat -> Nat
mult m Zero = Zero 
mult m (Succ n) = plus m (mult m n)

natToInt :: Nat -> Int 
natToInt Zero = 0 
natToInt (Succ n) = natToInt n + 1

intToNat :: Int -> Nat 
intToNat n = if n <= 0 then Zero else Succ (intToNat (n-1))

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
tokListToExp toklist                = e where 
    e:_                             = foldl step [] toklist
    step :: [Exp] -> Token -> [Exp] 
    step es             (Left n)    = Val n: es 
    step (exp2:exp1:es) (Right op)  = op exp1 exp2: es

strToExp :: String -> Exp 
strToExp    = tokListToExp . strToTokList

evalExpStr :: String -> Int 
evalExpStr  = eval . strToExp 



power :: Int -> Int -> Int 
power x 0 = 1 
power x n = x * power x (n-1)

infList :: [Integer]
infList = infFrom 0

infFrom :: Num t => t -> [t]
infFrom n = n:infFrom (n+1)

indexOf :: Eq a => a -> [a] -> Int
indexOf x l         = go x (zip [0..] l) where 
    go x []         = -1 
    go x ((i,y):ps) = if x == y then i else go x ps 

primes :: [Integer]
primes = sieve [2..] where 
    sieve (p:xs) = p: sieve [x | x <- xs, x `mod` p /= 0]
