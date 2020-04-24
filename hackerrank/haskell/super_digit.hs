{-# LANGUAGE BangPatterns #-}

import           Control.Applicative (liftA)
import           Data.Char           (digitToInt)
import           System.IO

superDigit :: String -> Integer
superDigit = helper 0 
  where
    helper !acc [] = if acc < 10 then acc else helper 0 (show acc)
    helper !acc !(head:tail) = helper (acc + fromIntegral (digitToInt head)) tail

main :: IO ()
main = do
    [superNumber, multStr] <- liftA words getLine
    let mult = read multStr :: Int
    print $ superDigit (show (superDigit superNumber * (fromIntegral mult)))
