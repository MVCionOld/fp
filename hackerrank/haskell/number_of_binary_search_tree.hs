import           Control.Applicative ((<$>))
import           Control.Monad       (forM_)

mod' :: Integer -> Integer
mod' = flip (mod) (10^8 + 7)

countBstWays :: Int -> Integer
countBstWays = (map ways [0..] !!)
  where 
    ways 0 = 1
    ways 1 = 1
    ways n = sum [
      (countBstWays $ i - 1) * (countBstWays $ n - i)
      | i <- [1..n]]

main :: IO ()
main = do 
  testCasesNum <- (\x -> read x :: Int) <$> getLine
  forM_ [0..testCasesNum - 1] $ \_ -> do
    nodesNum <- (\x -> read x :: Int) <$> getLine
    print $ mod' $ countBstWays nodesNum
