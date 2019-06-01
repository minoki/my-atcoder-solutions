-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
import Data.Char
import Data.Int
import Data.List
import Control.Monad
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS
import Data.Monoid

exEuclid :: (Eq a, Integral a) => a -> a -> (a, a, a)
exEuclid !f !g = loop 1 0 0 1 f g
  where loop !u0 !u1 !v0 !v1 !f 0 = (f, u0, v0)
        loop !u0 !u1 !v0 !v1 !f g =
          case divMod f g of
            (q,r) -> loop u1 (u0 - q * u1) v1 (v0 - q * v1) g r

modulo = 10^6+3 :: Int64
addMod, subMod, mulMod, divM :: Int64 -> Int64 -> Int64
addMod !x !y = (x + y) `rem` modulo
subMod !x !y = (x - y) `mod` modulo
mulMod !x !y = (x * y) `rem` modulo
recipM :: Int64 -> Int64
recipM !x = case exEuclid x modulo of
             (1,a,_) -> a `mod` modulo
             (-1,a,_) -> (-a) `mod` modulo
divM !x !y = x `mulMod` recipM y

newtype N = N { unwrapN :: Int64 } deriving (Eq)
instance Show N where
  show (N x) = show x
instance Num N where
  N x + N y = N ((x + y) `rem` modulo)
  N x - N y = N ((x - y) `mod` modulo)
  N x * N y = N ((x * y) `rem` modulo)
  fromInteger n = N (fromInteger (n `mod` fromIntegral modulo))
  abs = undefined; signum = undefined

instance Fractional N where
  N x / N y = N (divM x y)
  recip (N x) = N (recipM x)
  fromRational = undefined

factL :: [N]
factL = 1 : zipWith (*) factL (map fromInteger [1..])
factFrom :: Integer -> N -> [N]
factFrom n v0 = let xs = v0 : zipWith (*) xs (map fromInteger [n+1..])
                in xs
factV :: V.Vector N
factV = sl 200000 factL
  <> sl (500000 - 200000) (factFrom 200000 177247)
  <> sl (700000 - 500000) (factFrom 500000 2)
  <> sl (total - 700000) (factFrom 700000 426941)
  where
    total = fromIntegral modulo :: Int
    sl n xs = V.fromListN n (take n xs)
factM :: Int64 -> N
factM n | n < modulo = factV V.! fromIntegral n
        | otherwise = 0

solve :: Int -> Int -> Int -> N
solve x 0 n = (fromIntegral x)^n
solve !x !d !n = let !x' = fromIntegral x :: N
                     !d' = fromIntegral d :: N
                     !xd = unwrapN (x' / d')
                 in if xd + fromIntegral n - 1 >= modulo
                    then 0
                    else d'^n * factM (xd + fromIntegral n - 1) / factM (xd - 1)

main = do
  q :: Int <- readLn
  replicateM_ q $ do
    [x,d,n] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    print (solve x d n)
