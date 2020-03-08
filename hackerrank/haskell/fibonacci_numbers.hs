--Contributed by Ron Watkins
module Main where


fib :: Int -> Int
fib n = helper 0 1 n
  where
    helper prev curr n   
        | n == 1    = prev
        | n == 2    = curr
        | otherwise = helper curr (curr + prev) (n - 1)


-- This part is related to the Input/Output and can be used as it is
-- Do not modify it
main = do
    input <- getLine
    print . fib . (read :: String -> Int) $ input
