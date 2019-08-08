-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
import Data.Char (isSpace)
import Data.Int
import Data.List (unfoldr)
import Data.Coerce
import Control.Monad
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS
---
import qualified Data.Vector.Generic
import qualified Data.Vector.Generic.Mutable

main = do
  n <- readLn
  edges <- U.replicateM (n-1) $ do
    [x,y] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    return (x-1,y-1)
  let edges' :: V.Vector [Int]
      edges' = V.create $ do
        vec <- VM.replicate n []
        U.forM_ edges $ \(i,j) -> do
          VM.modify vec (j :) i
          VM.modify vec (i :) j
        return vec
  let resultVec :: U.Vector N
      resultVec = U.create $ do
        vec <- UM.replicate n invalidN
        let go !i !j = do
              x <- UM.read vec j
              if x /= invalidN
                then return x
                else do x <- if null (edges' V.! j)
                             then pure 2
                             else liftM2 (+) (productM [ go j k | k <- edges' V.! j, k /= i ])
                                             (productM [ go k l | k <- edges' V.! j, k /= i, l <- edges' V.! k, l /= j ])
                        UM.write vec j x
                        return x
        go (-1) 0
        return vec
  print $ resultVec U.! 0

sumM :: (Monad m, Num a) => [m a] -> m a
sumM = foldM (\x m -> (x +) <$> m) 0

productM :: (Monad m, Num a) => [m a] -> m a
productM = foldM (\x m -> (x *) <$> m) 1

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

invalidN = N (-1)

{-# RULES
"^9/Int" forall x. x ^ (9 :: Int) = let u = x; v = u * u * u in v * v * v
"^9/Integer" forall x. x ^ (9 :: Integer) = let u = x; v = u * u * u in v * v * v
 #-}

---

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
