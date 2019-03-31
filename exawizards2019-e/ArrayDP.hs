{-# LANGUAGE BangPatterns #-}
module ArrayDP where
import Control.Monad
import Data.Int
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS
import Data.Array.IO

exEuclid :: (Eq a, Integral a) => a -> a -> (a, a, a)
exEuclid f g = loop 1 0 0 1 f g
  where loop u0 u1 v0 v1 f 0 = (f, u0, v0)
        loop u0 u1 v0 v1 f g =
          case divMod f g of
            (q,r) -> loop u1 (u0 - q * u1) v1 (v0 - q * v1) g r

modulo = 10^9+7 :: Int64
addMod, subMod, mulMod, divM :: Int64 -> Int64 -> Int64
addMod x y = (x + y) `rem` modulo
subMod x y = (x - y) `mod` modulo
mulMod x y = (x * y) `rem` modulo
recipM :: Int64 -> Int64
recipM x = case exEuclid x modulo of
             (1,a,_) -> a `mod` modulo
             (-1,a,_) -> (-a) `mod` modulo
divM x y = x `mulMod` recipM y

newtype N = N Int64 deriving (Eq, Show)
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

main = do
  [b,w] <- map (read . BS.unpack) . BS.words <$> BS.getLine
  let half = 1 / 2 :: N
  arr <- newArray ((0,0),(b+1,w+1)) 0 :: IO (IOUArray (Int,Int) Int64)
  writeArray arr (b,w) 1
  vec <- UM.new (b+w) :: IO (UM.IOVector Int64)
  forM_ [b+w-1,b+w-2..0] $ \i -> do
    forM_ [max 0 (i - w)..min b i] $ \j -> do
      let k = i - j
      -- j + k == i
      -- 0 <= j <= b, 0 <= k <= w
      u <- readArray arr (j+1,k)
      v <- readArray arr (j,k+1)
      let N uu | k == 0 = N u
               | otherwise = half * N u
      let N vv | j == 0 = N v
               | otherwise = half * N v
      UM.modify vec (`addMod` uu) i
      writeArray arr (j,k) (uu `addMod` vv)
  {-
  forM_ [0..b] $ \j -> do
    r <- forM [0..w] $ \k -> readArray arr (j,k)
    print r
  -}
  forM_ [b+w-1,b+w-2..0] $ \i -> do
    print =<< UM.read vec i
