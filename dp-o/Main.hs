-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
import Data.Char (isSpace)
import Data.Int (Int64)
import Data.Bits
import Data.List (unfoldr)
import Data.Coerce
import Control.Monad
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Char8 as BS
import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad.ST
---
import qualified Data.Array.Base
import qualified Unsafe.Coerce

main = do
  n <- readLn -- 1 <= n <= 21
  xs <- V.replicateM n $ do
    U.unfoldrN n (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let result :: UArray Int N
      result = runSTUArray $ do
        arr <- asSTUArray $ newArray (0,2^n-1) invalidN
        writeArray arr 0 1
        -- loop i y: i 番目以降の男性の集合と女性の集合 y に関して、組み合わせの数を計算する。
        --           y に関しては、 Int をビットの集合とみなす。
        -- Invariant: i + popCount y == n
        -- loop :: Int -> Int -> ST s N
        let loop !i !y | i == n = return 1
            loop !i !y = do
              -- x, y: 相手が決まっていない男性、女性の集合
              z <- readArray arr y
              if z /= invalidN
                then return z
                else do s <- foldM (\x a -> (x +) <$> a) 0
                             [ loop (i + 1) (clearBit y j)
                             | j <- [0..n-1]
                             , testBit y j
                             , (xs V.! i) U.! j == 1
                             ]
                        writeArray arr y s
                        return s
        loop 0 (2^n-1)
        return arr
  -- print result
  print $ result ! (2^n-1)

---

modulo :: Int64
modulo = 10^9+7
addMod, subMod, mulMod :: Int64 -> Int64 -> Int64
addMod !x !y | x + y >= modulo = x + y - modulo
             | otherwise = x + y
subMod !x !y | x >= y = x - y
             | otherwise = x - y + modulo
mulMod !x !y = (x * y) `rem` modulo

newtype N = N { unwrapN :: Int64 } deriving (Eq)
instance Show N where
  show (N x) = show x
instance Num N where
  (+) = coerce addMod
  (-) = coerce subMod
  (*) = coerce mulMod
  fromInteger n = N (fromInteger (n `mod` fromIntegral modulo))
  abs = undefined; signum = undefined
invalidN = N modulo

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

--- STUArray s i N

asSTUArray :: ST s (STUArray s i a) -> ST s (STUArray s i a)
asSTUArray = id

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
