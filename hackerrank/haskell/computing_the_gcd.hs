module Main where


gcd' :: Integral a => a -> a -> a
gcd' n m = helper n m
  where
    helper n m
        | n == 0    = m
        | m == 0    = n
        | otherwise = helper m (mod n m)
    


-- This part is related to the Input/Output and can be used as it is
-- Do not modify it
main = do
  input <- getLine
  print . uncurry gcd' . listToTuple . convertToInt . words $ input
 where
  listToTuple (x:xs:_) = (x,xs)
  convertToInt = map (read :: String -> Int)
