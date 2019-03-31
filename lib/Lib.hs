{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Lib where
import Control.Monad
import Data.Int
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS
import Data.Array.Unboxed
import Data.Array.IO
import Data.Array.ST
import Control.Monad.ST
import Data.Coerce
import qualified Data.Vector.Generic
import qualified Data.Vector.Generic.Mutable
import qualified Data.Array.Base
import qualified Unsafe.Coerce

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

unsafeCoerce_UArray_N_Int :: UArray i N -> UArray i Int64
unsafeCoerce_UArray_N_Int = Unsafe.Coerce.unsafeCoerce
unsafeCoerce_UArray_Int_N :: UArray i Int64 -> UArray i N
unsafeCoerce_UArray_Int_N = Unsafe.Coerce.unsafeCoerce

instance Data.Array.Base.IArray UArray N where
  bounds arr = Data.Array.Base.bounds (unsafeCoerce_UArray_N_Int arr)
  numElements arr = Data.Array.Base.numElements (unsafeCoerce_UArray_N_Int arr)
  unsafeArray lu ies = unsafeCoerce_UArray_Int_N $ Data.Array.Base.unsafeArray lu (coerce ies)
  unsafeAt arr i = coerce (Data.Array.Base.unsafeAt (unsafeCoerce_UArray_N_Int arr) i)
  unsafeReplace arr ies = unsafeCoerce_UArray_Int_N (Data.Array.Base.unsafeReplace (unsafeCoerce_UArray_N_Int arr) (coerce ies))
  unsafeAccum f arr ies = unsafeCoerce_UArray_Int_N (Data.Array.Base.unsafeAccum (coerce f) (unsafeCoerce_UArray_N_Int arr) ies)
  unsafeAccumArray f e lu ies = unsafeCoerce_UArray_Int_N (Data.Array.Base.unsafeAccumArray (coerce f) (coerce e) lu ies)

unsafeCoerce_IOUArray_N_Int :: IOUArray i N -> IOUArray i Int64
unsafeCoerce_IOUArray_N_Int = Unsafe.Coerce.unsafeCoerce
unsafeCoerce_IOUArray_Int_N :: IOUArray i Int64 -> IOUArray i N
unsafeCoerce_IOUArray_Int_N = Unsafe.Coerce.unsafeCoerce

instance Data.Array.Base.MArray IOUArray N IO where
  getBounds arr = Data.Array.Base.getBounds (unsafeCoerce_IOUArray_N_Int arr)
  getNumElements arr = Data.Array.Base.getNumElements (unsafeCoerce_IOUArray_N_Int arr)
  newArray lu e = unsafeCoerce_IOUArray_Int_N <$> Data.Array.Base.newArray lu (coerce e)
  newArray_ lu = unsafeCoerce_IOUArray_Int_N <$> Data.Array.Base.newArray_ lu
  unsafeNewArray_ lu = unsafeCoerce_IOUArray_Int_N <$> Data.Array.Base.unsafeNewArray_ lu
  unsafeRead arr i = coerce <$> Data.Array.Base.unsafeRead (unsafeCoerce_IOUArray_N_Int arr) i
  unsafeWrite arr i e = Data.Array.Base.unsafeWrite (unsafeCoerce_IOUArray_N_Int arr) i (coerce e)

unsafeCoerce_STUArray_N_Int :: STUArray s i N -> STUArray s i Int64
unsafeCoerce_STUArray_N_Int = Unsafe.Coerce.unsafeCoerce
unsafeCoerce_STUArray_Int_N :: STUArray s i Int64 -> STUArray s i N
unsafeCoerce_STUArray_Int_N = Unsafe.Coerce.unsafeCoerce

instance Data.Array.Base.MArray (STUArray s) N (ST s) where
  getBounds arr = Data.Array.Base.getBounds (unsafeCoerce_STUArray_N_Int arr)
  getNumElements arr = Data.Array.Base.getNumElements (unsafeCoerce_STUArray_N_Int arr)
  newArray lu e = unsafeCoerce_STUArray_Int_N <$> Data.Array.Base.newArray lu (coerce e)
  newArray_ lu = unsafeCoerce_STUArray_Int_N <$> Data.Array.Base.newArray_ lu
  unsafeNewArray_ lu = unsafeCoerce_STUArray_Int_N <$> Data.Array.Base.unsafeNewArray_ lu
  unsafeRead arr i = coerce <$> Data.Array.Base.unsafeRead (unsafeCoerce_STUArray_N_Int arr) i
  unsafeWrite arr i e = Data.Array.Base.unsafeWrite (unsafeCoerce_STUArray_N_Int arr) i (coerce e)


hoge :: UArray Int N
hoge = array (1,1) [(1,N 5)]
