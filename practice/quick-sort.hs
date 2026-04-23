sort :: [Int] -> [Int]
sort [] = []
sort (x:xs) = sort front ++ [pivot] ++ sort back where
    pivot = x
    front = [x | x<- xs, x <= pivot]
    back = [x | x <- xs, x > pivot]