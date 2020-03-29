import Control.Applicative (liftA)
import Control.Monad       (forM_)
import Data.Vector         (fromList,Vector,(!))

mod' :: Integer -> Integer
mod' arg = mod arg (10^8 + 7)

diffWaysMem :: Vector (Vector Integer)
diffWaysMem  = fromList [ 
    fromList [ 
        helper k n | k <- [ 0 .. 1000 ] ] 
        | n <- [ 0 .. 1000 ] ]
  where
    helper k n
      | k == 0    = 1
      | k == n    = 1
      | otherwise = mod' $ diffWaysMem  ! (n-1) ! (k-1) 
      + (mod' $ diffWaysMem  ! (n-1) ! k)

getDiffWaysNum :: Int -> Int -> Int
getDiffWaysNum n k = fromIntegral $ mod' $ (diffWaysMem ! n ! k)

main :: IO ()
main = do
  testCasesNum <- readLn
  forM_ [ 1 .. testCasesNum ] $ \_ -> do
    [ n, k ] <- liftA (map (\x -> read x :: Int) . words) getLine
    print $ getDiffWaysNum n k

