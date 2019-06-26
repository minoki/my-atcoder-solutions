{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module UnboxedModularArray where
import ModularArithmetic
---
import Data.Int
import Data.Coerce
import Data.Array.Unboxed (UArray)
import Data.Array.IO (IOUArray)
import Control.Monad.ST (ST)
import Data.Array.ST (STUArray)
---
import qualified Data.Array.Base
import qualified Unsafe.Coerce

asSTUArray :: ST s (STUArray s i a) -> ST s (STUArray s i a)
asSTUArray = id

--- UArray i N

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

--- IOUArray i N

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

--- STUArray s i N

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
