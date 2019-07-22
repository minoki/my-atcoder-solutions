-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Int
import Data.Char
import Data.Bits
import qualified Data.ByteString.Char8 as BS
import Numeric

readBin :: String -> Integer
readBin s = case readInt 2 (\c -> c == '0' || c == '1') digitToInt s of
              [(a, _)] -> a

naive :: String -> N
naive s = let l = readBin s
          in sum [ 1
                 | a <- [0..l]
                 , b <- [0..l - a]
                 , a `xor` b == a + b
                 ]

main = do
  l <- BS.getLine
  let loop :: BS.ByteString -> N
      loop s = case BS.uncons s of
                   Just ('0', s') -> loop s'
                   Just ('1', s') -> 2 * loop s' + 3^(BS.length s')
                   Nothing -> 1
  print $ loop l

---

modulo = 10^9+7 :: Int64
addMod, subMod, mulMod :: Int64 -> Int64 -> Int64
addMod !x !y = (x + y) `rem` modulo
subMod !x !y = (x - y) `mod` modulo
mulMod !x !y = (x * y) `rem` modulo

newtype N = N { unwrapN :: Int64 } deriving (Eq)
instance Show N where
  show (N x) = show x
instance Num N where
  N x + N y = N ((x + y) `rem` modulo)
  N x - N y = N ((x - y) `mod` modulo)
  N x * N y = N ((x * y) `rem` modulo)
  fromInteger n = N (fromInteger (n `mod` fromIntegral modulo))
  abs = undefined; signum = undefined
