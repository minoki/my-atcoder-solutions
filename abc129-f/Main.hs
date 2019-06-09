-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Data.Char (isSpace)
import Data.Int (Int64)
import Data.List (unfoldr)
import qualified Data.ByteString.Char8 as BS
import Data.Proxy
import Data.Array.Unboxed
import Data.Coerce
---
import Unsafe.Coerce
import qualified Data.Array.Base

naive :: Integer -> Integer -> Integer -> Integer -> Integer
naive !l !a !b !m = read (concatMap show [a + b * i | i <- [0..l-1] ]) `rem` m

solve :: forall m. IsInt64 m => Proxy m -> Int64 -> Int64 -> Int64 -> Int64
solve proxy l a b =
  let kk :: [Int64]
      kk = [max 0 (min l ((10^(d-1) - a + b - 1) `quot` b)) | d <- [1..19]]
      a' = fromIntegral a :: IntMod m
      b' = fromIntegral b :: IntMod m
  in getIntMod $
     sum [ ((a' + fromIntegral k' * b') * arr1!(2,3) - b' * arr2!(1,3)) * 10^e
         | (d,k,k') <- zip3 [1..18] kk (tail kk)
         , k /= k'
         , let t :: IntMod m
               t = 10^d
               mat :: Mat3x3 UArray (IntMod m)
               mat = mkMat3x3 [[t,1,0]
                              ,[0,t,1]
                              ,[0,0,1]
                              ]
               mat'@(Mat3x3 arr1) = mat ^ (k' - k)
               Mat3x3 arr2 = mat' * mat
               e = sum $ drop d $ zipWith (*) [1..18] $ zipWith (-) (tail kk) kk
         ]

main = do
  [l,a,b,m] <- unfoldr (BS.readInteger . BS.dropWhile isSpace) <$> BS.getLine
  let result = reifyInt64 (fromInteger m) solve (fromInteger l) (fromInteger a) (fromInteger b)
  print result
  -- print $ naive l a b m

---

newtype Mat3x3 array a = Mat3x3 (array (Int,Int) a)

mkMat3x3 :: (IArray array a) => [[a]] -> Mat3x3 array a
mkMat3x3 xs = Mat3x3 $ array ((1,1),(3,3))
              [ ((i,j),y)
              | (i,ys) <- zip [1..3] xs
              , (j,y) <- zip [1..3] ys
              ]

instance (IArray array a, Num a) => Num (Mat3x3 array a) where
  Mat3x3 a + Mat3x3 b = Mat3x3 $ array ((1,1),(3,3))
                        [ (ij, a!ij + b!ij)
                        | ij <- range ((1,1),(3,3))
                        ]
  Mat3x3 a - Mat3x3 b = Mat3x3 $ array ((1,1),(3,3))
                        [ (ij, a!ij - b!ij)
                        | ij <- range ((1,1),(3,3))
                        ]
  Mat3x3 a * Mat3x3 b = Mat3x3 $ array ((1,1),(3,3))
                        [ ((i,k), v)
                        | (i,k) <- range ((1,1),(3,3))
                        , let !v = sum [a!(i,j) * b!(j,k) | j <- [1..3]]
                        ]
  fromInteger n = let !n' = fromInteger n :: a
                  in Mat3x3 $ array ((1,1),(3,3))
                     [ ((i,j), if i == j then n' else 0)
                     | (i,j) <- range ((1,1),(3,3))
                     ]
  abs = undefined; signum = undefined

---

newtype IntMod m = IntMod { getIntMod :: Int64 } deriving Eq
instance Show (IntMod m) where
  show (IntMod x) = show x
instance IsInt64 m => Num (IntMod m) where
  t@(IntMod x) + IntMod y = IntMod ((x + y) `rem` int64Val t)
  t@(IntMod x) - IntMod y = IntMod ((x - y) `mod` int64Val t)
  t@(IntMod x) * IntMod y = IntMod ((x * y) `rem` int64Val t)
  negate t@(IntMod x) = let m = int64Val t in IntMod ((m - x) `rem` m)
  fromInteger n = IntMod $ fromInteger $ n `mod` fromIntegral (int64Val (Proxy :: Proxy m))
  abs = undefined; signum = undefined

---

newtype Tagged tag a = Tagged { getTagged :: a }

class IsInt64 tag where
  taggedInt64Val :: Tagged tag Int64

int64Val :: forall proxy tag. IsInt64 tag => proxy tag -> Int64
int64Val _ = getTagged (taggedInt64Val :: Tagged tag Int64)

---

-- See Data.Reflection
newtype MagicInt64 a = MagicInt64 (forall tag. IsInt64 tag => Proxy tag -> a)
reifyInt64 :: forall a. Int64 -> (forall tag. IsInt64 tag => Proxy tag -> a) -> a
reifyInt64 x f = unsafeCoerce (MagicInt64 f :: MagicInt64 a) x Proxy

---

unsafeCoerce_UArray_IntMod_Int :: UArray i (IntMod m) -> UArray i Int64
unsafeCoerce_UArray_IntMod_Int = Unsafe.Coerce.unsafeCoerce
unsafeCoerce_UArray_Int_IntMod :: UArray i Int64 -> UArray i (IntMod m)
unsafeCoerce_UArray_Int_IntMod = Unsafe.Coerce.unsafeCoerce

instance Data.Array.Base.IArray UArray (IntMod m) where
  bounds arr = Data.Array.Base.bounds (unsafeCoerce_UArray_IntMod_Int arr)
  numElements arr = Data.Array.Base.numElements (unsafeCoerce_UArray_IntMod_Int arr)
  unsafeArray lu ies = unsafeCoerce_UArray_Int_IntMod $ Data.Array.Base.unsafeArray lu (coerce ies)
  unsafeAt arr i = coerce (Data.Array.Base.unsafeAt (unsafeCoerce_UArray_IntMod_Int arr) i)
  unsafeReplace arr ies = unsafeCoerce_UArray_Int_IntMod (Data.Array.Base.unsafeReplace (unsafeCoerce_UArray_IntMod_Int arr) (coerce ies))
  unsafeAccum f arr ies = unsafeCoerce_UArray_Int_IntMod (Data.Array.Base.unsafeAccum (coerce f) (unsafeCoerce_UArray_IntMod_Int arr) ies)
  unsafeAccumArray f e lu ies = unsafeCoerce_UArray_Int_IntMod (Data.Array.Base.unsafeAccumArray (coerce f) (coerce e) lu ies)
