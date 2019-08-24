-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Int (Int64)
import Data.Coerce
import qualified Data.ByteString.Char8 as BS
import Control.Exception (assert)

main = do
  n <- readLn
  s <- BS.getLine
  assert (BS.length s == 2 * n) $ return ()
  let loop :: N -> Int -> Int -> N
      loop !acc !k !i | BS.length s == i = if k == 0 then acc else 0
                      | BS.index s i == 'W' = if even k
                                              then loop (acc * fromIntegral k) (k-1) (i+1)
                                              else loop acc (k+1) (i+1)
                      | otherwise = if even k
                                    then loop acc (k+1) (i+1)
                                    else loop (acc * fromIntegral k) (k-1) (i+1)
  print $ loop (product $ map fromIntegral [1..n]) 0 0

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

fromIntegral_Int64_N :: Int64 -> N
fromIntegral_Int64_N n | 0 <= n && n < modulo = N n
                       | otherwise = N (n `mod` modulo)

{-# RULES
"fromIntegral/Int->N" fromIntegral = fromIntegral_Int64_N . (fromIntegral :: Int -> Int64)
"fromIntegral/Int64->N" fromIntegral = fromIntegral_Int64_N
 #-}
