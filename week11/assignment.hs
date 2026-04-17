import Control.Monad (replicateM)

act :: Bool -> IO Bool
act m = do
  b <- readLn :: IO Bool
  return (m || b)

main = do
  inp <- readLn :: IO Bool
  list <- replicateM 5 (act inp)
  if (and list) then return inp else return (not inp)

f = do
  x <- readLn :: IO Int
  if x == 0
    then return 0
    else do
      y <- f
      return (x + y)