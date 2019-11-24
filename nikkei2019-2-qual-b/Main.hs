-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.Int (Int64)
import Data.List
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Coerce

main = do
  n <- readLn :: IO Int
  x0:xs <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let loop acc prev prevN (x:xs) | prev + 1 == x = let (x',xss) = span (== x) xs
                                                       m = length x' + 1
                                                   in loop (acc * prevN ^ m) x (fromIntegral m) xss
                                 | otherwise = 0
      loop acc _ _ [] = acc
  if x0 == 0 then
    print (loop 1 0 1 (sort xs) :: N)
  else
    print 0

--
-- Modular Arithmetic
--

modulo :: Int64
modulo = 998244353
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

fromIntegral_Int64_N :: Int64 -> N
fromIntegral_Int64_N n | 0 <= n && n < modulo = N n
                       | otherwise = N (n `mod` modulo)

{-# RULES
"fromIntegral/Int->N" fromIntegral = fromIntegral_Int64_N . (fromIntegral :: Int -> Int64)
"fromIntegral/Int64->N" fromIntegral = fromIntegral_Int64_N
 #-}
