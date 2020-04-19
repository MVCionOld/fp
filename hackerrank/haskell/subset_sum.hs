import           Control.Applicative (liftA)
import           Control.Monad       (forM_)
import           Data.List           (scanr, sort)
import           Data.Ord
import qualified Data.Set            as S (Set, fromList, size, split)
import           System.IO

subsetSum :: S.Set Int -> Int -> Int
subsetSum subset threshold = let (subset', _) = S.split threshold subset
              in S.size subset'

main :: IO ()
main = do
  _ <- readLn :: IO Int
  list <- liftA (map (\x -> read x :: Int)) (liftA words getLine)
  let sums = scanr (+) 0 $ sort list
  let subset = S.fromList sums
  testCasesNum <- readLn :: IO Int
  forM_ [1 .. testCasesNum] $ \_ -> do
    threshold <- readLn :: IO Int
    if threshold > head sums
      then putStrLn "-1"
    else 
      print $ subsetSum subset threshold

