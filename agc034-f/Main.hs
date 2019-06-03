-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
import Data.Char (isSpace)
import Data.Bits (xor)
import Control.Monad (forM_)
-- import qualified Data.Vector as V
-- import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS
import Data.Array (Array)
import Data.Array.Unboxed (UArray)
import Data.Array.IArray
---
import Data.Coerce
import qualified Data.Vector.Generic
import qualified Data.Vector.Generic.Mutable
import qualified Data.Array.Base
import qualified Unsafe.Coerce

type NN = N
type Mat = UArray (Int,Int) NN
type Vec = U.Vector NN
{-
type NN = Rational
type Mat = Array (Int,Int) Rational
type Vec = V.Vector NN
-}

main = do
  n :: Int <- readLn -- 1 <= n <= 18
  as <- U.unfoldrN (2^n) (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let s = U.sum as :: Int
      !as' = U.map fromIntegral as :: Vec
      !s' = recip (fromIntegral s) :: NN
      coeffMat :: Mat
      coeffMat = array ((1,1),(2^n-1,2^n)) $
        [ ((i,j),v)
        | i <- [1..2^n-1]
        , j <- [1..2^n-1]
        , let v | i == j = 1 - (as' U.! 0) * s'
                | otherwise = - (as' U.! (i `xor` j)) * s'
        ]
        ++ [((i,2^n),1) | i <- [1..2^n-1]]
  let result = solve coeffMat
  print 0
  forM_ [1..2^n-1] $ \j -> do
    print $ result!(j,2^n) / result!(j,j)

-- 行基本変形を行う
solve :: (Eq k, Fractional k, IArray arr k) => arr (Int,Int) k -> arr (Int,Int) k
solve m | i0 == j0 = loop i0 m
        | otherwise = error "not supported"
  where
    b@((i0,j0),(iN,_)) = bounds m
    loop !i !m
      | i > iN = m
      | otherwise = case [(k,w) | k <- [i..iN], let w = m!(k,i), w /= 0] of
          (!k,!w):_ -> let !r = recip w -- r == recip (m!(k,i))
                       in loop (i+1) $ array b
                          [ ((if i' == k then i else if i' == i then k else i',j),v)
                          | (i',j) <- indices m
                          , let v | i' == k = m!(i',j)
                                  | otherwise = m!(i',j) - m!(k,j)*m!(i',i)*r
                          ]
          [] -> error "singular matrix"

---

exEuclid :: (Eq a, Integral a) => a -> a -> (a, a, a)
exEuclid !f !g = loop 1 0 0 1 f g
  where loop !u0 !u1 !v0 !v1 !f 0 = (f, u0, v0)
        loop !u0 !u1 !v0 !v1 !f g =
          case divMod f g of
            (q,r) -> loop u1 (u0 - q * u1) v1 (v0 - q * v1) g r

modulo = 998244353 :: Int
addMod, subMod, mulMod, divM :: Int -> Int -> Int
addMod !x !y = (x + y) `rem` modulo
subMod !x !y = (x - y) `mod` modulo
mulMod !x !y = (x * y) `rem` modulo
recipM :: Int -> Int
recipM !x = case exEuclid x modulo of
             (1,a,_) -> a `mod` modulo
             (-1,a,_) -> (-a) `mod` modulo
divM !x !y = x `mulMod` recipM y

newtype N = N { unwrapN :: Int } deriving (Eq)
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

---

newtype instance UM.MVector s N = MV_N (UM.MVector s Int)
newtype instance U.Vector N = V_N (U.Vector Int)

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

unsafeCoerce_UArray_N_Int :: UArray i N -> UArray i Int
unsafeCoerce_UArray_N_Int = Unsafe.Coerce.unsafeCoerce
unsafeCoerce_UArray_Int_N :: UArray i Int -> UArray i N
unsafeCoerce_UArray_Int_N = Unsafe.Coerce.unsafeCoerce

instance Data.Array.Base.IArray UArray N where
  bounds arr = Data.Array.Base.bounds (unsafeCoerce_UArray_N_Int arr)
  numElements arr = Data.Array.Base.numElements (unsafeCoerce_UArray_N_Int arr)
  unsafeArray lu ies = unsafeCoerce_UArray_Int_N $ Data.Array.Base.unsafeArray lu (coerce ies)
  unsafeAt arr i = coerce (Data.Array.Base.unsafeAt (unsafeCoerce_UArray_N_Int arr) i)
  unsafeReplace arr ies = unsafeCoerce_UArray_Int_N (Data.Array.Base.unsafeReplace (unsafeCoerce_UArray_N_Int arr) (coerce ies))
  unsafeAccum f arr ies = unsafeCoerce_UArray_Int_N (Data.Array.Base.unsafeAccum (coerce f) (unsafeCoerce_UArray_N_Int arr) ies)
  unsafeAccumArray f e lu ies = unsafeCoerce_UArray_Int_N (Data.Array.Base.unsafeAccumArray (coerce f) (coerce e) lu ies)

