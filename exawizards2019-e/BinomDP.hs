{-# LANGUAGE BangPatterns #-}
module BinomDP where
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

newtype N = N { unwrapN :: Int64 } deriving (Eq, Show)
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
      minBW = min b w
  let addModHalf :: Int64 -> Int64 -> Int64
      addModHalf !x !y = addMod x y `mulMod` unwrapN half -- (x + y) / 2
      at k vec j | j < U.length vec = N (vec U.! j)
                 | j <= k, k-j < U.length vec = N (vec U.! (k-j))
                 | otherwise = 0
      {-
      tweakedBinomNext !vec = if U.length vec < minBW
                              then U.zipWith addModHalf (U.snoc vec 0) (U.cons 0 vec)
                              else U.zipWith addModHalf vec (U.cons 0 $ U.take (minBW - 1) vec)
      -}
      tweakedBinomNext !vec = U.create $ do
        vec' <- UM.new (min (U.length vec + 1) minBW)
        UM.write vec' 0 1
        forM_ [1..min (minBW - 1) (U.length vec - 1)] $ \i -> do
          UM.write vec' i $ (vec U.! (i-1)) `addModHalf` (vec U.! i)
        when (U.length vec < minBW) $ UM.write vec' (U.length vec) 1
        return vec'
      loop !k !x !vec = do
        print $ unwrapN x
        if k == b+w-1
          then return ()
          else loop (k+1) (x + at k vec (w-1) - at k vec (b-1)) (tweakedBinomNext vec)
          {-
      tweakedBinom :: [U.Vector Int64]
      tweakedBinom = U.singleton (unwrapN $ half * half)
                     : map (\vec -> if U.length vec < minBW
                                    then U.zipWith addModHalf (U.snoc vec 0) (U.cons 0 vec)
                                    else U.zipWith addModHalf vec (U.cons 0 $ U.take (minBW - 1) vec)) tweakedBinom
      solution = scanl (\x (km1,vec) -> x + at km1 vec (w-1) - at km1 vec (b-1)) half (zip [0..b+w-2] tweakedBinom)
  forM_ (solution) $ \x -> do
    print $ unwrapN x
-}
  loop 0 half $ U.singleton (unwrapN $ half * half)
