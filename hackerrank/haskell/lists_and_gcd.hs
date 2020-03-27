import Control.Applicative ((<$>))
import Control.Monad       (forM)
import Data.Maybe

newtype Power = Power (Int, Int)
  deriving Show

readNumbers :: String -> [Power]
readNumbers str = helper [] (map (\x -> read x :: Int) $ words str) 
  where
    helper ps []       = ps
    helper ps (x:y:ys) = helper (Power (x,y) : ps) ys

listGcd :: [[Power]] -> [Power]
listGcd = foldr1 reduce

reduce :: [Power] -> [Power] -> [Power]
reduce = reduce' [] 
  where
    reduce' rs _ []      = rs
    reduce' rs [] _      = rs
    reduce' rs (x:xs) ys = reduce' (reduce'' x ys ++ rs) xs ys 
      where
        reduce'' x [] = []
        reduce'' x (y:ys) = case reducePower x y of
          Nothing -> reduce'' x ys
          Just v -> [v]

reducePower :: Power -> Power -> Maybe Power
reducePower (Power (x,y)) (Power (x',y'))
  | x == x' && y > y'  = Just (Power (x, y'))
  | x == x' && y < y'  = Just (Power (x, y))
  | x == x' && y == y' = Just (Power (x, y))
  | otherwise          = Nothing

showGcd :: [Power] -> String
showGcd []                          = []
showGcd [Power (arg0, arg1)]        = show arg0 ++ " " ++ show arg1
showGcd ((Power (arg0, arg1)):gcds) = show arg0 ++ " " ++ show arg1 ++ " " ++ showGcd gcds

main :: IO ()
main = do
  lstLen <- (\x -> read x :: Int) <$> getLine
  ns <- forM [1 .. lstLen] $ \_ -> do
    getLine
  putStrLn . showGcd . listGcd $ map readNumbers ns
