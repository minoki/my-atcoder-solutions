{-# LANGUAGE BangPatterns #-}
module SegmentTree where
import Data.Monoid
import Data.Bits
import Control.Applicative (liftA2)
import qualified Data.Vector.Generic.Mutable as GM
import Control.Monad.Primitive
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed.Mutable as UM

--
-- Segment Tree
--

data SegTree mvec s a = SegTree {-# UNPACK #-} !Int !(mvec s a)

queryAt_SegTree :: (GM.MVector mvec a, PrimMonad m) => SegTree mvec (PrimState m) a -> Int -> m a
queryAt_SegTree (SegTree depth vec) !i = GM.read vec ((1 `shiftL` depth) - 1 + i)
{-# INLINE queryAt_SegTree #-}

-- queryRange_SegTree i j tree == mconcat <$> sequence [queryAt_SegTree tree k | k <- [i..j-1]]
queryRange_SegTree :: (Monoid a, GM.MVector mvec a, PrimMonad m) => SegTree mvec (PrimState m) a -> Int -> Int -> m a
queryRange_SegTree (SegTree depth vec) !i !j | i < j = doQuery 0 depth i j
                                             | otherwise = return mempty
  where
    -- Invariant: 0 <= k*2^l <= i < j <= (k+1)*2^l <= 2^depth
    doQuery !k 0 !i !j | i == k, j == k+1 = GM.read vec ((1 `shiftL` depth) - 1 + k)
                       | otherwise = error "queryRange"
    doQuery !k l !i !j | i == k `shiftL` l, j == (k+1) `shiftL` l = GM.read vec ((1 `shiftL` (depth-l)) - 1 + k)
                       | m <= i = doQuery (2*k+1) (l-1) i j
                       | j <= m = doQuery (2*k) (l-1) i j
                       | otherwise = liftA2 (<>) (doQuery (2*k) (l-1) i m) (doQuery (2*k+1) (l-1) m j)
      where m = (2*k+1) `shiftL` (l-1)
{-# INLINE queryRange_SegTree #-}

update_SegTree :: (Monoid a, GM.MVector mvec a, PrimMonad m) => SegTree mvec (PrimState m) a -> Int -> a -> m ()
update_SegTree (SegTree depth vec) !i !x = loop ((1 `shiftL` depth) + i) x
  where
    loop 1 !x = GM.write vec 0 x
    loop !j !x = do
      GM.write vec (j - 1) x
      y <- if even j
           then (x <>) <$> GM.read vec j
           else (<> x) <$> GM.read vec (j - 2)
      loop (j `shiftR` 1) y
{-# INLINE update_SegTree #-}

new_SegTree :: (Monoid a, GM.MVector mvec a, PrimMonad m) => Int -> m (SegTree mvec (PrimState m) a)
new_SegTree n = do let depth = ceiling (logBase 2 (fromIntegral n) :: Double) :: Int
                   vec <- GM.replicate ((1 `shiftL` (depth + 1)) - 1) mempty
                   return (SegTree depth vec)
{-# INLINE new_SegTree #-}

asBoxedSegTree :: (PrimMonad m) => m (SegTree VM.MVector (PrimState m) a) -> m (SegTree VM.MVector (PrimState m) a)
asBoxedSegTree = id

asUnboxedSegTree :: (PrimMonad m) => m (SegTree UM.MVector (PrimState m) a) -> m (SegTree UM.MVector (PrimState m) a)
asUnboxedSegTree = id
