-- type Complex = (Double, Double)

data Day = Sun | Mon | Tue | Wed | Thu | Fri | Sat 
    deriving (Eq, Ord, Enum, Show)

data Shape = Circle Double | Square Double | Rectangle Double Double 
    deriving (Eq)

instance Show Shape where
    show :: Shape -> String 
    show (Circle r)         = "Circle with radius " ++ show r 
    show (Square s)         = "Square with side length " ++ show s
    show (Rectangle l b)    = "Rectangle with length " ++ show l ++ " and breadth " ++ show b 

instance Ord Shape where 
    (<=) :: Shape -> Shape -> Bool
    shape1 <= shape2 = area shape1 <= area shape2 

shape1, shape2, shape3 :: Shape
shape1 = Circle 3.0 
shape2 = Square 4.0
shape3 = Rectangle 4.0 3.0 

myshapes :: [Shape]
myshapes = [shape1, shape2, shape3]

weekend :: Day -> Bool
weekend Sat = True 
weekend Sun = True 
weekend _   = False 

weekend2 :: Day -> Bool 
weekend2 d = d == Sat || d == Sun 

nextday :: Day -> Day 
nextday Sun = Mon 
nextday Mon = Tue 
nextday Tue = Wed 
nextday Wed = Thu 
nextday Thu = Fri 
nextday Fri = Sat 
nextday Sat = Sun 

nextday2 :: Day -> Day 
nextday2 Sat    = Sun 
nextday2 d      = succ d 

prevday2 :: Day -> Day 
prevday2 Sun    = Sat 
prevday2 d      = pred d

alldays :: [Day]
alldays = [Sun .. Sat]

area :: Shape -> Double 
area (Circle r)         = pi * r * r 
area (Square s)         = s * s
area (Rectangle l b)    = l * b 

-- data Complex = Complex Double Double 
--     deriving (Eq, Show)

-- instance Ord Complex where 
--     (<=) :: Complex -> Complex -> Bool
--     cmplx1 <= cmplx2    = absVal cmplx1 <= absVal cmplx2

-- absVal :: Complex -> Double 
-- absVal (Complex x y)    = sqrt (x^2 + y^2)

-- re, im :: Complex -> Double 
-- re (Complex x _)    = x
-- im (Complex _ y)    = y 

data Complex = Complex {re :: Double, im :: Double} 
    deriving Eq 

instance Show Complex where 
    show :: Complex -> String
    show cmplx = show (re cmplx) ++ " + i " ++ show (im cmplx)

instance Ord Complex where
    (<=) :: Complex -> Complex -> Bool
    cmplx1 <= cmplx2    = absVal cmplx1 <= absVal cmplx2 

absVal :: Complex -> Double 
absVal cmplx = sqrt (re cmplx^2 + im cmplx^2)

cmplx1, cmplx2 :: Complex
cmplx1 = Complex {im = 4.0, re = 3.0}
cmplx2 = Complex 3.0 4.0 

data List a = Nil | Cons a (List a) 
    deriving (Eq, Ord)

instance Show a => Show (List a) where 
    show :: Show a => List a -> String
    show l = "[" ++ go l ++ "]" where 
        go Nil = ""
        go (Cons x Nil) = show x 
        go (Cons x l) = show x ++ ", " ++ go l