import           Control.Applicative (liftA)
import qualified Data.Vector         as V


procList :: Int -> [Int] -> V.Vector Int
procList minElem = helper (V.replicate 100 0) 
  where
    helper acc []                = acc
    helper acc (vecHead:vecTail) = helper acc' vecTail
      where
        acc' = V.accum (+) acc [(vecHead - minElem, 1)]
                                     


main :: IO ()
main = do
  n <- liftA (\x -> read x :: Int) getLine
  list1 <- liftA words getLine
  m <- liftA (\x -> read x :: Int) getLine
  list2 <- liftA words getLine
  let listA = map (\x -> read x :: Int) list1
  let listB = map (\x -> read x :: Int) list2
  let minElem = min (minimum listA) (minimum listB)
  let listA' = procList minElem listA
  let listB' = procList minElem listB
  let diffs = V.fromList [minElem .. minElem + 100]
  V.mapM_ print'
    (V.zipWith3 pseudoFilter listA' listB' diffs) 
      where
        pseudoFilter a b c = if b > a then c else 0
        print' x = 
          if x /= 0 then 
            putStr (show x ++ " ")
          else 
            return ()

