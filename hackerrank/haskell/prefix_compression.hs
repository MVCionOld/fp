longestPrefix :: String -> String -> String
longestPrefix = helper [] where
  helper acc [] _ = reverse acc
  helper acc _ [] = reverse acc
  helper acc (x:xs) (y:ys)
    | x == y    = helper (x:acc) xs ys
    | otherwise = reverse acc

subString :: String -> String -> (Int, String)
subString xs prefix = let s = drop (length prefix) xs
                      in  (length s, s)

main :: IO ()
main = do
  x <- getLine
  y <- getLine
  let lgstPref = longestPrefix x y
  putStrLn $ show (length lgstPref) ++ " " ++ lgstPref
  putStrLn $ show (fst (subString x lgstPref)) ++ " " ++ snd (subString x lgstPref)
  putStrLn $ show (fst (subString y lgstPref)) ++ " " ++ snd (subString y lgstPref)
