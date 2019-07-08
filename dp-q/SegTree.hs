-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
import Data.Char (isSpace)
import Data.Int (Int64)
import Data.List (unfoldr)
import Data.Bifunctor (first)
import Control.Monad
import Control.Monad.ST
import Control.Applicative (liftA2)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS
import Data.Bits
import Data.Coerce
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import Control.Monad.Primitive
import Data.Monoid
#ifdef MIN_VERSION_base
#if MIN_VERSION_base(4,9,0)
import Data.Semigroup hiding ((<>),Max(..),Min(..))
#endif
#endif
import Debug.Trace

main = do
  n <- readLn
  hs <- U.unfoldrN n (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  as <- U.unfoldrN n (readInt64 . BS.dropWhile isSpace) <$> BS.getLine
  let result = runST $ do
        tree <- asUnboxedSegTree $ new_SegTree n
        update_SegTree tree 0 (Max 0)
        forM_ [0..n-1] $ \i -> do
          let h = hs U.! i
          Max x <- queryRange_SegTree tree 0 h
          update_SegTree tree (h - 1) (Max (x + as U.! i))
        getMax <$> queryRange_SegTree tree 0 n
  print result

readInt64 :: BS.ByteString -> Maybe (Int64, BS.ByteString)
readInt64 s = first fromIntegral <$> BS.readInt s

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

asUnboxedSegTree :: (PrimMonad m) => m (SegTree UM.MVector (PrimState m) a) -> m (SegTree UM.MVector (PrimState m) a)
asUnboxedSegTree = id

--
-- Max monoid (from Data.Semigroup)
--

newtype Max a = Max { getMax :: a }
#ifdef MIN_VERSION_base
#if MIN_VERSION_base(4,9,0)
instance (Ord a) => Semigroup (Max a) where
  Max x <> Max y = Max (x `max` y)
#endif
#endif
instance (Bounded a, Ord a) => Monoid (Max a) where
  mempty = Max minBound
  Max x `mappend` Max y = Max (x `max` y)

--
-- instance U.Unbox (Max a)
--

newtype instance UM.MVector s (Max a) = MV_Max (UM.MVector s a)
newtype instance U.Vector (Max a) = V_Max (U.Vector a)

instance GM.MVector UM.MVector a => GM.MVector UM.MVector (Max a) where
  basicLength (MV_Max mv) = GM.basicLength mv
  basicUnsafeSlice i l (MV_Max mv) = MV_Max (GM.basicUnsafeSlice i l mv)
  basicOverlaps (MV_Max mv) (MV_Max mv') = GM.basicOverlaps mv mv'
  basicUnsafeNew l = MV_Max <$> GM.basicUnsafeNew l
  basicInitialize (MV_Max mv) = GM.basicInitialize mv
  basicUnsafeReplicate i x = MV_Max <$> GM.basicUnsafeReplicate i (coerce x)
  basicUnsafeRead (MV_Max mv) i = coerce <$> GM.basicUnsafeRead mv i
  basicUnsafeWrite (MV_Max mv) i x = GM.basicUnsafeWrite mv i (coerce x)
  basicClear (MV_Max mv) = GM.basicClear mv
  basicSet (MV_Max mv) x = GM.basicSet mv (coerce x)
  basicUnsafeCopy (MV_Max mv) (MV_Max mv') = GM.basicUnsafeCopy mv mv'
  basicUnsafeMove (MV_Max mv) (MV_Max mv') = GM.basicUnsafeMove mv mv'
  basicUnsafeGrow (MV_Max mv) n = MV_Max <$> GM.basicUnsafeGrow mv n

instance G.Vector U.Vector a => G.Vector U.Vector (Max a) where
  basicUnsafeFreeze (MV_Max mv) = V_Max <$> G.basicUnsafeFreeze mv
  basicUnsafeThaw (V_Max v) = MV_Max <$> G.basicUnsafeThaw v
  basicLength (V_Max v) = G.basicLength v
  basicUnsafeSlice i l (V_Max v) = V_Max (G.basicUnsafeSlice i l v)
  basicUnsafeIndexM (V_Max v) i = coerce <$> G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Max mv) (V_Max v) = G.basicUnsafeCopy mv v
  elemseq (V_Max v) x y = G.elemseq v (coerce x) y

instance U.Unbox a => U.Unbox (Max a)
