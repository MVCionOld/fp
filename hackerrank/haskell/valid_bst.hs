import           Control.Applicative (liftA)
import           Control.Monad       (forM_)

data Stack a = Empty
  | StackElem a (Stack a)
    deriving (Eq, Show)

empty :: Stack a
empty = Empty

push :: a -> Stack a -> Stack a
push elem Empty = StackElem elem Empty
push elem stack = StackElem elem stack

pop :: Stack a -> (Maybe a, Stack a)
pop Empty = (Nothing, Empty)
pop (StackElem elem stack) = (Just elem, stack)

peek :: Stack a -> Maybe a
peek = fst . pop

isValidBST :: [Int] -> Bool
isValidBST = go empty (minBound :: Int) 
  where
    go _ _ []                 = True
    go stack minV (elem:elems)
      | elem < minV           = False
      | stack == Empty        = go (push elem stack) minV elems
      | otherwise             =
        let Just stackTop = peek stack
        in if stackTop < elem then 
             let (Just minV', stack') = pop stack
                  in go stack' minV' (elem:elems)
             else go (push elem stack) minV elems

main :: IO ()
main = do
  testCasesNum <- readLn
  forM_ [1..testCasesNum] $ \_ -> do
    _ <- getLine
    elems <- liftA (map (\x -> read x :: Int) . words) getLine
    putStrLn $ if isValidBST elems then "YES" else "NO"
