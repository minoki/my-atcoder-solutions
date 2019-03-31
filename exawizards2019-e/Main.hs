{-# LANGUAGE BangPatterns #-}
import Control.Monad
import Data.Int
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS
import Data.Array.IO

exEuclid :: (Eq a, Integral a) => a -> a -> (a, a, a)
exEuclid !f !g = loop 1 0 0 1 f g
  where loop !u0 !u1 !v0 !v1 !f 0 = (f, u0, v0)
        loop !u0 !u1 !v0 !v1 !f g =
          case divMod f g of
            (q,r) -> loop u1 (u0 - q * u1) v1 (v0 - q * v1) g r

modulo = 10^9+7 :: Int64
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

-- binomColumn j !! k == binom k j
binomColumn :: Fractional a => Int -> [a]
binomColumn !j = replicate j 0 ++ (1 : loop 1 (j+1))
  where loop !x !k = let !y = x * fromIntegral k / fromIntegral (k - j)
                     in y : loop y (k + 1)

-- tweakedBinomColumn j !! k == binom k j * (1/2)^(k+2)
tweakedBinomColumn :: Fractional a => Int -> [a]
tweakedBinomColumn !j = replicate j 0 ++ (half^(j+2) : loop (half^(j+2)) (j+1))
  where loop !x !k = let !y = x * fromIntegral k / (2 * (fromIntegral $ k - j))
                     in y : loop y (k + 1)
        half = 1/2

main = do
  [b,w] <- map (read . BS.unpack) . BS.words <$> BS.getLine
  let solution = scanl (\x (y,z) -> x + y - z) (1/2) (zip (tweakedBinomColumn (w-1)) (tweakedBinomColumn (b-1)))
  forM_ (take (b + w) solution) $ \x -> do
    print $ unwrapN x
