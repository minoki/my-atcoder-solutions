-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
import Data.Int (Int64)
import qualified Data.ByteString.Char8 as BS
import Data.Coerce

main = do
  n :: Int <- readLn
  s <- BS.getLine
  let loop :: Int -> Int -> Int -> N
      loop !t !k !i | BS.length s == i = if k == 0 then 1 else 0
                    | BS.index s i == 'W' = if k == 0
                                            then 0
                                            else if even k
                                                 then fromIntegral k * loop t (k-1) (i+1)
                                                 else fromIntegral (t+1) * loop (t+1) (k+1) (i+1)
                    | otherwise = if k == 0
                                  then fromIntegral (t+1) * loop (t+1) 1 (i+1)
                                  else if even k
                                       then fromIntegral (t+1) * loop (t+1) (k+1) (i+1)
                                       else fromIntegral k * loop t (k-1) (i+1)
  print $ loop 0 0 0

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
