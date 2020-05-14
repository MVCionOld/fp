import           Control.Applicative ((<$>))

minMultiple :: Integral a => a -> a -> a
minMultiple a b = a * b `div` (gcd a b)

main :: IO ()
main = do
  bunniesNum <- (\x -> read x :: Int) <$> getLine
  distances <- words <$> getLine
  print . foldr1 minMultiple . map (\x -> read x :: Int) $ take bunniesNum distances
