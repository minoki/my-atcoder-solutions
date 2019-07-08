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

main = do
  n <- readLn
  hs <- U.unfoldrN n (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  as <- U.unfoldrN n (readInt64 . BS.dropWhile isSpace) <$> BS.getLine
  let result = runST $ do
        tree <- asUnboxedBIT $ new_BIT n
        add_BIT tree 0 (Max 0)
        forM_ [0..n-1] $ \i -> do
          let h = hs U.! i
          Max x <- queryM_BIT tree h
          add_BIT tree (h - 1) (Max (x + as U.! i))
        getMax <$> queryM_BIT tree n
  print result

readInt64 :: BS.ByteString -> Maybe (Int64, BS.ByteString)
readInt64 s = first fromIntegral <$> BS.readInt s

--
-- Binary Indexed Tree (BIT)
--

type CommutativeMonoid a = Monoid a

newtype BIT mvec s a = BIT (mvec s a)

-- index: 1-based
-- property: forall vec i. fromVector_BIT vec >>= flip queryM_BIT i == pure (G.scanl (<>) mempty vec G.! i)
queryM_BIT :: (Monoid a, GM.MVector mvec a, PrimMonad m) => BIT mvec (PrimState m) a -> Int -> m a
queryM_BIT (BIT vec) !i = doQuery i mempty
  where
    doQuery 0 !acc = return acc
    doQuery i !acc = do y <- GM.read vec (i - 1)
                        let !j = (i - 1) .&. i
                        doQuery j (y <> acc)

-- index: zero-based
-- property: forall vec i x. do { tree <- fromVector_BIT vec; add_BIT tree i x; return tree } == fromVector_BIT (G.accum (<>) vec [(i,x)])
add_BIT :: (CommutativeMonoid a, GM.MVector mvec a, PrimMonad m) => BIT mvec (PrimState m) a -> Int -> a -> m ()
add_BIT (BIT vec) !i !y = loop (i + 1)
  where
    loop !k | k > GM.length vec = return ()
    loop !k = do x <- GM.read vec (k - 1)
                 GM.write vec (k - 1) $! x <> y
                 loop (k + (k .&. (-k)))

new_BIT :: (Monoid a, GM.MVector mvec a, PrimMonad m) => Int -> m (BIT mvec (PrimState m) a)
new_BIT n = BIT <$> GM.replicate n mempty

asUnboxedBIT :: (PrimMonad m) => m (BIT UM.MVector (PrimState m) a) -> m (BIT UM.MVector (PrimState m) a)
asUnboxedBIT = id

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
