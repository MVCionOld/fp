import           Control.Applicative    (liftA)
import           Control.Monad          (forM_, when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State    (StateT, evalStateT, get, put)
import           Data.List              (foldl1')
import qualified Data.Vector            as V (Vector, fromList, length, modify, toList, (!))
import qualified Data.Vector.Mutable    as V (write)
import           System.IO


mod' :: Int -> Int
mod' arg = mod arg (10^9 + 7)

productMod :: [Int] -> Int
productMod [] = 1
productMod vec = foldl1' (\x y -> mod' (x * y)) vec

main :: IO ()
main = do
    listALen <- liftA (\x -> read x :: Int) getLine
    listA <- liftA words getLine
    listBLen <- liftA (\x -> read x :: Int) getLine
    listB <- liftA words getLine
    let vectorA = V.fromList $ map (\x -> read x :: Int) listA
    let vectorB = V.fromList $ map (\x -> read x :: Int) listB
    flip evalStateT (vectorA, vectorB, []) $ do
      forM_ [0 .. listALen - 1] $ \i ->
        forM_ [0 .. listBLen - 1] $ \j -> do
          (numerVec, denomVec, gcds) <- get
          let numer = numerVec V.! i
          let denom = denomVec V.! j
          when (gcd numer denom /= 1) $ do
            let vectorA' = V.modify (\v -> V.write v i (numer `div` gcd numer denom)) numerVec
            let vectorB' = V.modify (\v -> V.write v j (denom `div` gcd numer denom)) denomVec
            put (vectorA', vectorB', gcd numer denom : gcds)
      (finalVecA, finalVecB, gcds) <- get
      liftIO . print $ productMod gcds

