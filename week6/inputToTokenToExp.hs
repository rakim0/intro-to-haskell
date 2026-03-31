data Exp = Val Int | Add Exp Exp | Sub Exp Exp | Mul Exp Exp | Div Exp Exp deriving (Eq)

-- doing infix sorta thing
instance Show Exp where
  show (Val n) = show n
  show (Add expl expr) = "(" ++ show expl ++ " + " ++ show expr ++ ")"
  show (Sub expl expr) = "(" ++ show expl ++ " - " ++ show expr ++ ")"
  show (Mul expl expr) = "(" ++ show expl ++ " * " ++ show expr ++ ")"
  show (Div expl expr) = "(" ++ show expl ++ " / " ++ show expr ++ ")"

eval :: Exp -> Int
eval (Val n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = eval e1 `div` eval e2

-- parsing postfix
type Token = Either Int (Exp -> Exp -> Exp)

strToTokList :: String -> [Token]
strToTokList str = map tokenize (words str)
  where
    tokenize :: String -> Token
    tokenize "+" = Right Add
    tokenize "-" = Right Sub
    tokenize "*" = Right Mul
    tokenize "/" = Right Div
    tokenize str = Left (read str :: Int)

tokListToExp :: [Token] -> Exp
tokListToExp toks = e
  where
    (e : _) = foldl step [] toks
      where
        step es (Left n) = Val n : es
        step (e1 : e2 : es) (Right op) = op e1 e2 : es

strToExp :: String -> Exp
strToExp = tokListToExp . strToTokList

evalExpStr :: String -> Int
evalExpStr = eval . strToExp

main :: IO ()
main = do
  print (eval (Add (Val 3) (Mul (Val 5) (Val 2))))