isOrdered :: Int -> Int -> Int -> Bool
isOrdered x y z = x <= y && y <= z

xor1, xor2, xor3, xor4, xor5, xor6, xor7, xor8, xor9, xor10, xor11, xor12, xor13, xor14, xor15, xor16, xor17 :: Bool -> Bool -> Bool 

xor1 b1 b2 = (b1 && not b2) || (not b1 && b2) 

xor2 b1 b2 = if b1 == b2 then False else True 

xor3 b1 b2 = if b1 /= b2 then True else False 

xor4 b1 b2 = b1 /= b2 

xor5 b1 b2 = (/=) b1 b2 

xor6 = (/=)

xor7 False False = False 
xor7 False True = True  
xor7 True False = True 
xor7 True True = False 

xor8 False True = True 
xor8 True False = True 
xor8 b1 b2 = False

xor9 False b = b
xor9 b False = b 
xor9 b1 b2 = False 

xor10 False b = b 
xor10 True b = not b 

xor11 False True = True 
xor11 True False = True 
xor11 _ _ = False

xor12 b1 b2 
    | b1 == b2 = False 
    | b1 /= b2 = True 

xor13 False b = b 
xor13 True b 
    | not b = True 
    | b     = False 

xor14 b1 b2 
    | b1        = not b2 
    | not b2    = b1 

xor15 b1 b2 
    | b1    = not b2 
    | otherwise = b2 

xor16 b1 b2 = case b1 of 
    False -> b2 
    True -> not b2 

xor17 b1 b2 = case b1 of 
    False -> case b2 of 
                False -> False 
                True -> True 
    True -> b2 
