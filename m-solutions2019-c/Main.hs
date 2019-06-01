-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Data.Char
import Data.Int
import Data.List
import Control.Monad
import Control.Monad.ST
import qualified Data.ByteString.Char8 as BS
import Data.Array.Unboxed
import Data.Array.ST
import qualified Data.Array.Base
import qualified Unsafe.Coerce
import Data.Coerce

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

main = do
  [n,a,b,c] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let a', b', c' :: N
      a' = fromIntegral a / fromIntegral (100 - c)
      b' = fromIntegral b / fromIntegral (100 - c)
      c' = 100 / fromIntegral (100 - c)
  let arr :: UArray (Int, Int) N
      arr = runSTUArray $ do
        arr <- newArray ((0,0),(n,n)) 0
        forM_ [1..n] $ \i -> do
          forM_ [1..n] $ \j -> do
            x <- readArray arr (i-1,j)
            y <- readArray arr (i,j-1)
            writeArray arr (i,j) $ a'*x+b'*y+c'
        return arr
  print $ arr ! (n,n)

---

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
