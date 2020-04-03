import Control.Monad       (forM_)
import System.IO
  

main :: IO ()
main = do
  testCasesNum <- readLn :: IO Int
  forM_ [ 1 .. testCasesNum ] $ \_ -> do
    n <- readLn :: IO Int
    print $ (n * (3*n-1)) `div` 2

