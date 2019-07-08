{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module MonoidEx where
import Data.Monoid
import Data.Coerce
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Generic
import qualified Data.Vector.Generic.Mutable
#ifdef MIN_VERSION_base
#if MIN_VERSION_base(4,9,0)
import Data.Semigroup hiding ((<>),Max(..),Min(..))
#endif
#endif

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
-- Min monoid (from Data.Semigroup)
--

newtype Min a = Min { getMin :: a }
#ifdef MIN_VERSION_base
#if MIN_VERSION_base(4,9,0)
instance (Ord a) => Semigroup (Min a) where
  Min x <> Min y = Min (x `min` y)
#endif
#endif
instance (Bounded a, Ord a) => Monoid (Min a) where
  mempty = Min maxBound
  Min x `mappend` Min y = Min (x `min` y)

--
-- instance U.Unbox (Max a)
--

newtype instance UM.MVector s (Max a) = MV_Max (UM.MVector s a)
newtype instance U.Vector (Max a) = V_Max (U.Vector a)

instance Data.Vector.Generic.Mutable.MVector UM.MVector a => Data.Vector.Generic.Mutable.MVector UM.MVector (Max a) where
  basicLength (MV_Max mv) = Data.Vector.Generic.Mutable.basicLength mv
  basicUnsafeSlice i l (MV_Max mv) = MV_Max (Data.Vector.Generic.Mutable.basicUnsafeSlice i l mv)
  basicOverlaps (MV_Max mv) (MV_Max mv') = Data.Vector.Generic.Mutable.basicOverlaps mv mv'
  basicUnsafeNew l = MV_Max <$> Data.Vector.Generic.Mutable.basicUnsafeNew l
  basicInitialize (MV_Max mv) = Data.Vector.Generic.Mutable.basicInitialize mv
  basicUnsafeReplicate i x = MV_Max <$> Data.Vector.Generic.Mutable.basicUnsafeReplicate i (coerce x)
  basicUnsafeRead (MV_Max mv) i = coerce <$> Data.Vector.Generic.Mutable.basicUnsafeRead mv i
  basicUnsafeWrite (MV_Max mv) i x = Data.Vector.Generic.Mutable.basicUnsafeWrite mv i (coerce x)
  basicClear (MV_Max mv) = Data.Vector.Generic.Mutable.basicClear mv
  basicSet (MV_Max mv) x = Data.Vector.Generic.Mutable.basicSet mv (coerce x)
  basicUnsafeCopy (MV_Max mv) (MV_Max mv') = Data.Vector.Generic.Mutable.basicUnsafeCopy mv mv'
  basicUnsafeMove (MV_Max mv) (MV_Max mv') = Data.Vector.Generic.Mutable.basicUnsafeMove mv mv'
  basicUnsafeGrow (MV_Max mv) n = MV_Max <$> Data.Vector.Generic.Mutable.basicUnsafeGrow mv n

instance Data.Vector.Generic.Vector U.Vector a => Data.Vector.Generic.Vector U.Vector (Max a) where
  basicUnsafeFreeze (MV_Max mv) = V_Max <$> Data.Vector.Generic.basicUnsafeFreeze mv
  basicUnsafeThaw (V_Max v) = MV_Max <$> Data.Vector.Generic.basicUnsafeThaw v
  basicLength (V_Max v) = Data.Vector.Generic.basicLength v
  basicUnsafeSlice i l (V_Max v) = V_Max (Data.Vector.Generic.basicUnsafeSlice i l v)
  basicUnsafeIndexM (V_Max v) i = coerce <$> Data.Vector.Generic.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Max mv) (V_Max v) = Data.Vector.Generic.basicUnsafeCopy mv v
  elemseq (V_Max v) x y = Data.Vector.Generic.elemseq v (coerce x) y

instance U.Unbox a => U.Unbox (Max a)

--
-- instance U.Unbox (Min a)
--

newtype instance UM.MVector s (Min a) = MV_Min (UM.MVector s a)
newtype instance U.Vector (Min a) = V_Min (U.Vector a)

instance Data.Vector.Generic.Mutable.MVector UM.MVector a => Data.Vector.Generic.Mutable.MVector UM.MVector (Min a) where
  basicLength (MV_Min mv) = Data.Vector.Generic.Mutable.basicLength mv
  basicUnsafeSlice i l (MV_Min mv) = MV_Min (Data.Vector.Generic.Mutable.basicUnsafeSlice i l mv)
  basicOverlaps (MV_Min mv) (MV_Min mv') = Data.Vector.Generic.Mutable.basicOverlaps mv mv'
  basicUnsafeNew l = MV_Min <$> Data.Vector.Generic.Mutable.basicUnsafeNew l
  basicInitialize (MV_Min mv) = Data.Vector.Generic.Mutable.basicInitialize mv
  basicUnsafeReplicate i x = MV_Min <$> Data.Vector.Generic.Mutable.basicUnsafeReplicate i (coerce x)
  basicUnsafeRead (MV_Min mv) i = coerce <$> Data.Vector.Generic.Mutable.basicUnsafeRead mv i
  basicUnsafeWrite (MV_Min mv) i x = Data.Vector.Generic.Mutable.basicUnsafeWrite mv i (coerce x)
  basicClear (MV_Min mv) = Data.Vector.Generic.Mutable.basicClear mv
  basicSet (MV_Min mv) x = Data.Vector.Generic.Mutable.basicSet mv (coerce x)
  basicUnsafeCopy (MV_Min mv) (MV_Min mv') = Data.Vector.Generic.Mutable.basicUnsafeCopy mv mv'
  basicUnsafeMove (MV_Min mv) (MV_Min mv') = Data.Vector.Generic.Mutable.basicUnsafeMove mv mv'
  basicUnsafeGrow (MV_Min mv) n = MV_Min <$> Data.Vector.Generic.Mutable.basicUnsafeGrow mv n

instance Data.Vector.Generic.Vector U.Vector a => Data.Vector.Generic.Vector U.Vector (Min a) where
  basicUnsafeFreeze (MV_Min mv) = V_Min <$> Data.Vector.Generic.basicUnsafeFreeze mv
  basicUnsafeThaw (V_Min v) = MV_Min <$> Data.Vector.Generic.basicUnsafeThaw v
  basicLength (V_Min v) = Data.Vector.Generic.basicLength v
  basicUnsafeSlice i l (V_Min v) = V_Min (Data.Vector.Generic.basicUnsafeSlice i l v)
  basicUnsafeIndexM (V_Min v) i = coerce <$> Data.Vector.Generic.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Min mv) (V_Min v) = Data.Vector.Generic.basicUnsafeCopy mv v
  elemseq (V_Min v) x y = Data.Vector.Generic.elemseq v (coerce x) y

instance U.Unbox a => U.Unbox (Min a)
