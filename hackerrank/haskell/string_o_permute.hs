import Control.Applicative ((<$>))
import Control.Monad       (forM)

swap :: String -> String
swap = swap' [] where
  swap' rs [] = reverse rs
  swap' rs (x:y:ys) = swap' (x:y:rs) ys

main :: IO ()
main = do
  testCases <- (\x -> read x :: Int) <$> getLine
  strs <- forM [1..testCases] $ \_ -> do
    getLine
  mapM_ putStrLn (map swap strs)
