{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module UnboxedModularVector where
import ModularArithmetic
---
import Data.Int
import Data.Coerce
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
---
import qualified Data.Vector.Generic
import qualified Data.Vector.Generic.Mutable

newtype instance UM.MVector s N = MV_N (UM.MVector s Int64)
newtype instance U.Vector N = V_N (U.Vector Int64)

instance Data.Vector.Generic.Mutable.MVector UM.MVector N where -- needs MultiParamTypeClasses here
  basicLength (MV_N mv) = Data.Vector.Generic.Mutable.basicLength mv
  basicUnsafeSlice i l (MV_N mv) = MV_N (Data.Vector.Generic.Mutable.basicUnsafeSlice i l mv)
  basicOverlaps (MV_N mv) (MV_N mv') = Data.Vector.Generic.Mutable.basicOverlaps mv mv'
  basicUnsafeNew l = MV_N <$> Data.Vector.Generic.Mutable.basicUnsafeNew l
  basicInitialize (MV_N mv) = Data.Vector.Generic.Mutable.basicInitialize mv
  basicUnsafeReplicate i x = MV_N <$> Data.Vector.Generic.Mutable.basicUnsafeReplicate i (coerce x)
  basicUnsafeRead (MV_N mv) i = coerce <$> Data.Vector.Generic.Mutable.basicUnsafeRead mv i
  basicUnsafeWrite (MV_N mv) i x = Data.Vector.Generic.Mutable.basicUnsafeWrite mv i (coerce x)
  basicClear (MV_N mv) = Data.Vector.Generic.Mutable.basicClear mv
  basicSet (MV_N mv) x = Data.Vector.Generic.Mutable.basicSet mv (coerce x)
  basicUnsafeCopy (MV_N mv) (MV_N mv') = Data.Vector.Generic.Mutable.basicUnsafeCopy mv mv'
  basicUnsafeMove (MV_N mv) (MV_N mv') = Data.Vector.Generic.Mutable.basicUnsafeMove mv mv'
  basicUnsafeGrow (MV_N mv) n = MV_N <$> Data.Vector.Generic.Mutable.basicUnsafeGrow mv n

instance Data.Vector.Generic.Vector U.Vector N where -- needs MultiParamTypeClasses here
  basicUnsafeFreeze (MV_N mv) = V_N <$> Data.Vector.Generic.basicUnsafeFreeze mv
  basicUnsafeThaw (V_N v) = MV_N <$> Data.Vector.Generic.basicUnsafeThaw v
  basicLength (V_N v) = Data.Vector.Generic.basicLength v
  basicUnsafeSlice i l (V_N v) = V_N (Data.Vector.Generic.basicUnsafeSlice i l v)
  basicUnsafeIndexM (V_N v) i = coerce <$> Data.Vector.Generic.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_N mv) (V_N v) = Data.Vector.Generic.basicUnsafeCopy mv v
  elemseq (V_N v) x y = Data.Vector.Generic.elemseq v (coerce x) y

instance U.Unbox N
