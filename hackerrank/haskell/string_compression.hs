import Data.List (group)

compress :: String -> String
compress = concatMap helper . group where
  helper [] = []
  helper xs = let len = length xs
         in if (len == 1)
              then xs
              else head xs : show len

main :: IO ()
main = do
  msg <- getLine
  putStrLn $ compress msg
