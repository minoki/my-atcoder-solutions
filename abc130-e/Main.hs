-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Data.Char (isSpace)
import Data.Int
import Data.List (unfoldr)
import Control.Monad
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS
import Data.Array.ST
import Control.Monad.ST
---
import qualified Data.Array.Base
import qualified Unsafe.Coerce
import Data.Coerce

main = do
  [n,m] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  ss <- U.unfoldrN n (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  ts <- U.unfoldrN m (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let result :: N
      result = runST $ do
        arr <- asSTUArray $ newArray ((-1,-1),(n-1,m-1)) 0
        forM_ [-1..n-1] $ \i -> writeArray arr (i,-1) 1
        forM_ [-1..m-1] $ \j -> writeArray arr (-1,j) 1
        flip U.imapM_ ss $ \i s -> do
          flip U.imapM_ ts $ \j t -> do
            a <- readArray arr (i-1,j-1)
            b <- readArray arr (i-1,j)
            c <- readArray arr (i,j-1)
            writeArray arr (i,j) $! if s == t
                                    then b + c
                                    else b + c - a
        readArray arr (n-1,m-1)
  print result

---

modulo :: Int64
modulo = 10^9+7
addMod, subMod, mulMod :: Int64 -> Int64 -> Int64
addMod !x !y = (x + y) `rem` modulo
subMod !x !y = (x - y) `mod` modulo
mulMod !x !y = (x * y) `rem` modulo

newtype N = N { unwrapN :: Int64 } deriving (Eq)
instance Show N where
  show (N x) = show x
instance Num N where
  N x + N y = N ((x + y) `rem` modulo)
  N x - N y = N ((x - y) `mod` modulo)
  N x * N y = N ((x * y) `rem` modulo)
  fromInteger n = N (fromInteger (n `mod` fromIntegral modulo))
  abs = undefined; signum = undefined

---

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

asSTUArray :: ST s (STUArray s i a) -> ST s (STUArray s i a)
asSTUArray arr = arr
