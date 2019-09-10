-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.List
import qualified Data.ByteString.Char8 as BS

data SecondMaxInt = SecondMaxInt { getMaxS :: !Int, getSecondMax :: !Int }

-- Semigroup
cat :: SecondMaxInt -> SecondMaxInt -> SecondMaxInt
cat (SecondMaxInt a b) (SecondMaxInt c d)
  = case compare a c of
      LT -> SecondMaxInt c (max a d)
      EQ -> SecondMaxInt a (max b d)
      GT -> SecondMaxInt a (max b c)

singleton :: Int -> SecondMaxInt
singleton a = SecondMaxInt a minBound

main = do
  n <- readLn :: IO Int
  xs <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  print $ sum [getSecondMax y | ys <- tails xs, not (null ys), y <- tail $ scanl1 cat $ map singleton ys]
