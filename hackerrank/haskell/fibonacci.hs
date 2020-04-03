import Control.Applicative (liftA)
import Control.Monad       (forM_)
import Data.Vector         (fromList,Vector,(!))
import System.IO


mod' :: Integer -> Integer
mod' arg = mod arg (10^8 + 7)

fibonacciMemoization :: Vector Integer
fibonacciMemoization  = 
    fromList [ fibonacciMemoizationHelper k | k <- [ 0 .. 10^4 ] ] 
  where
    fibonacciMemoizationHelper n
      | n == 0    = 0
      | n == 1    = 1
      | otherwise = mod' $ (fibonacciMemoization ! (n-1)) 
      + (mod' $ fibonacciMemoization  ! (n-2))

fibonacci :: Int -> Int
fibonacci n = fromIntegral $ mod' $ (fibonacciMemoization ! n)

main :: IO ()
main = do
  testCasesNum <- readLn :: IO Int
  forM_ [ 1 .. testCasesNum ] $ \_ -> do
    n <- readLn :: IO Int
    print $ fibonacci n

