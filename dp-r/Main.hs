-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Data.Char (isSpace)
import Data.Int (Int64)
import Data.List (unfoldr)
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Array.Unboxed
import Data.Coerce
---
import qualified Data.Array.Base
import qualified Unsafe.Coerce

main = do
  [n',k'] <- unfoldr (BS.readInteger . BS.dropWhile isSpace) <$> BS.getLine
  let n :: Int
      n = fromInteger n'
      k :: Int64
      k = fromInteger k'
  elements <- replicateM n $ map fromIntegral . unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let mat :: UArray (Int,Int) N
      mat = array ((1,1),(n,n))
            [ ((i,j),a) | (i,e) <- zip [1..n] elements, (j,a) <- zip [1..n] e ]
  print $ sum $ elems $ matPow n mat k

---

matMul :: UArray (Int,Int) N -> UArray (Int,Int) N -> UArray (Int,Int) N
matMul a b = let ((i0,j0),(ix,jx)) = bounds a
                 ((j'0,k0),(j'x,kx)) = bounds b
             in if jx - j0 == j'x - j'0
                   then array ((i0,k0),(ix,kx))
                        [ ((i,k), sum [a!(i,j) * b!(j',k) | (j,j') <- zip (range (j0,jx)) (range (j'0,j'x))])
                        | i <- range (i0,ix)
                        , k <- range (k0,kx)
                        ]
                else error "Matrix size mismatch"

matPow :: Int -> UArray (Int,Int) N -> Int64 -> UArray (Int,Int) N
matPow k m 0 = array ((1,1),(k,k)) $
               [((i,j), if i == j then 1 else 0) | i <- [1..k], j <- [1..k]]
matPow _ m i = loop (i-1) m m
  where
    loop 0 !_ acc = acc
    loop 1 m acc = m `matMul` acc
    loop i m acc = case i `quotRem` 2 of
                     (j,0) -> loop j (m `matMul` m) acc
                     (j,_) -> loop j (m `matMul` m) (acc `matMul` m)

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

{-# RULES
"^9/Int" forall x. x ^ (9 :: Int) = let u = x; v = u * u * u in v * v * v
"^9/Integer" forall x. x ^ (9 :: Integer) = let u = x; v = u * u * u in v * v * v
 #-}

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
