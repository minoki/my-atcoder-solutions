-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.Int (Int64)
import Data.List (unfoldr)
import Data.Bits
import Data.Coerce
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS

int64Log2 :: Int64 -> Int
int64Log2 x = finiteBitSize x - countLeadingZeros x - 1

naive :: Int64 -> Int64 -> [(Int64, Int64)]
naive l r = [(x,y) | x <- [l..r], y <- [x..r], y `rem` x == y `xor` x]

solve :: Int64 -> Int64 -> N
solve l r | l > r  = 0
          | l == r = 1
          | log2l == log2r = solve2 l r log2l
          | otherwise = solveL l log2l + solveR r log2r + sum [3^i | i <- [log2l+1 .. log2r-1]]
  where
    log2l = int64Log2 l
    log2r = int64Log2 r
    solve2 l r i | int64Log2 l /= i || int64Log2 r /= i = error ("invalid " ++ show (l,r,i))
                 -- Precondition: l < r, int64Log2 l == int64Log2 r
                 | otherwise = loop i
      where loop i | i < 0 = 1
                   | otherwise = case (testBit l i, testBit r i) of
                                   (False, False) -> loop (i-1)
                                   (False, True) -> solveL l (i-1) + solveR r (i-1) + loop (i-1)
                                   (True, True) -> loop (i-1)
                                   (True, False) -> 0

solveL l i | i < 0 = 1
           | otherwise = if testBit l i
                         then solveL l (i-1)
                         else 2 * solveL l (i-1) + 3^i

solveR r i | i < 0 = 1
           | i == int64Log2 r = solveR r (i-1)
           | otherwise = if testBit r i
                         then 2 * solveR r (i-1) + 3^i
                         else solveR r (i-1)

main = do
  [l,r] <- map fromInteger . unfoldr (BS.readInteger . BS.dropWhile isSpace) <$> BS.getLine
  print $ solve l r

--
-- Modular Arithmetic
--

modulo :: Int64
modulo = 10^9+7
addMod, subMod, mulMod :: Int64 -> Int64 -> Int64
addMod !x !y | x + y >= modulo = x + y - modulo
             | otherwise = x + y
subMod !x !y | x >= y = x - y
             | otherwise = x - y + modulo
mulMod !x !y = (x * y) `rem` modulo

newtype N = N { unwrapN :: Int64 } deriving (Eq)
instance Show N where
  show (N x) = show x
instance Num N where
  (+) = coerce addMod
  (-) = coerce subMod
  (*) = coerce mulMod
  fromInteger n = N (fromInteger (n `mod` fromIntegral modulo))
  abs = undefined; signum = undefined

{-# RULES
"^9/Int" forall x. x ^ (9 :: Int) = let u = x; v = u * u * u in v * v * v
"^9/Integer" forall x. x ^ (9 :: Integer) = let u = x; v = u * u * u in v * v * v
 #-}
