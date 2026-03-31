head' :: [a] -> a
head' [] = error "can't call head on an empty"
head' (x:_) = x
