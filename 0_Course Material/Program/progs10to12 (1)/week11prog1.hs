import AVL
import Control.Monad ( replicateM )
import System.Random ( randomRIO )

randInts :: Int -> (Int, Int) -> IO [Int]
randInts sz (l,u)   = replicateM sz (randomRIO (l, u))

randAVL :: Int -> (Int, Int) -> IO (AVL Int)
randAVL sz (l,u) = createAVL <$> randInts sz (l,u)