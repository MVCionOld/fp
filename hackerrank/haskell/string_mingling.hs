zip'n'concate :: String -> String -> String
zip'n'concate a b = concat $ zipWith (\x y -> x : [y]) a b

main :: IO ()
main = do
  a <- getLine
  b <- getLine
  putStrLn $ zip'n'concate a b
