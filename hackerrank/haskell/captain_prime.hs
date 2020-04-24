import           Data.Char           (digitToInt)
import qualified Data.Set            as S (Set, fromList, member)
import           Control.Applicative ((<$>))
import           Control.Monad       (forM_)

data Fate = CENTRAL 
  | LEFT 
  | RIGHT 
  | DEAD 
  deriving (Eq, Show)

toDigits :: Int -> [Int]
toDigits = map digitToInt . show

digitsToInt :: [Int] -> Int
digitsToInt = foldl (\acc nxt -> 10 * acc + nxt) 0

primes :: Integral a => [a]
primes = 2 : sieve primes [3,5..] 
  where
    sieve []          _  = []
    sieve (div':divs) xs = let (head,tail) = span (< div' * div') xs
      in head ++ sieve divs (filter (\n -> mod n div' /= 0) tail)

cnvPrimeSet :: Integral a => (a -> Bool) -> S.Set a
cnvPrimeSet p = S.fromList (takeWhile p primes)

isPrime :: Integral a => S.Set a -> a -> Bool
isPrime = flip S.member

checkPrime :: Integral a => S.Set a -> a -> Fate
checkPrime s n
  | not (isPrime s n) || elem 0 digits = DEAD
  | otherwise = if leftPrime n then 
                  if not (rightPrime n) then LEFT
                  else CENTRAL
                else 
                  if rightPrime n then RIGHT
                  else DEAD
      where
        digits = toDigits (fromIntegral n)
        numDigits = length digits
        leftPrime n = all (==True) $
          map (\x -> isPrime s (mod n (10^x))) [1..numDigits - 1]
        rightPrime n = all (==True) $
          map (\x -> isPrime s (div n (10^x))) [1..numDigits - 1]

main :: IO ()
main = do
  testCasesNum <- (\x -> read x :: Int) <$> getLine
  let setOfPrimes = cnvPrimeSet (< 10^6)
  forM_ [0..testCasesNum - 1] $ \_ -> do
    id <- (\x -> read x :: Int) <$> getLine
    print $ checkPrime setOfPrimes id

