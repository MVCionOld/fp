import           Control.Applicative ((<$>))
import           Control.Monad       (forM_)
import           Data.List           (sortBy)
import qualified Data.Map            as M (filter, fromListWith, toList)
import           Data.Ord            (compare)
import           System.IO

repeatNums :: Int -> [Int] -> [(Int, (Int, Int))]
repeatNums repCntThreshold = sortBy (\(val0, (idx, repCnt0)) (val1, (idx', repCnt1)) -> compare idx idx') .
    M.toList .
    M.filter (\(_, repCnt) -> repCnt >= repCntThreshold) .
    M.fromListWith helper .
    zipWith (\x y -> (y, (x, 1))) [1..] where
  helper (idx, val) (idx', val')
    | idx <= idx'   = (idx, (val+val'))
    | otherwise = (idx', (val+val'))

dispNums :: [(Int, (Int, Int))] -> String
dispNums [] = "-1"
dispNums list = foldr (\(val, _) acc -> if null acc
                                        then show val
                                        else show val ++ " " ++ acc
                ) [] list

main :: IO ()
main = do
  testCasesNum <- (\x -> read x :: Int) <$> getLine
  forM_ [1..testCasesNum] $ \_ -> do
    [listLen, repCntThreshold] <- fmap (map (\x -> read x :: Int)) $ words <$> getLine
    list <- fmap (map (\x -> read x :: Int) . take listLen) $ words <$> getLine
    putStrLn . dispNums $ repeatNums repCntThreshold list
