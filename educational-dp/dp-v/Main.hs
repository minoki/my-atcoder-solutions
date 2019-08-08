-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
import Data.Char (isSpace)
import Data.Int (Int64)
import Data.List
import Control.Monad
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS
import Data.Proxy
import Data.Coerce
import Unsafe.Coerce
import qualified Data.Vector.Generic
import qualified Data.Vector.Generic.Mutable

buildGraph :: Int -> U.Vector (Int, Int) -> V.Vector [Int]
buildGraph !n edges = V.create $ do
  vec <- VM.replicate n []
  U.forM_ edges $ \(i,j) -> do
    VM.modify vec (j :) i
    VM.modify vec (i :) j
  return vec

solve :: forall m. IsInt64 m => Proxy m -> Int -> U.Vector (Int, Int) -> U.Vector (IntMod m)
solve _proxy !n edges =
  let graph = buildGraph n edges
      dp1 :: U.Vector (IntMod m)
      dp1 = U.create $ do
        vec <- UM.replicate n 0
        let go !parent !i = do
              v <- foldM (\ !x a -> (x *) <$> a) 1 [go i j | j <- graph V.! i, j /= parent]
              let !w = 1 + v
              UM.write vec i w
              return w
        go (-1) 0
        return vec
  in U.create $ do
    vec <- UM.replicate n 0
    let go !a !parent !i = do
          let children = filter (/= parent) $ graph V.! i
          let xs = scanl' (\ !x !i -> x * dp1 U.! i) 1 children
          let ys = scanr (\ !i !x -> dp1 U.! i * x) a children
          UM.write vec i (head ys)
          forM_ (zip3 children xs (tail ys)) $ \(j,s,t) -> do
            go (1 + s * t) i j
    go 1 (-1) 0
    return vec

main = do
  [n,m] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  edges <- U.replicateM (n-1) $ do
    [x,y] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    return (x-1,y-1)
  let result = reifyInt64 (fromIntegral m) (\proxy -> case solve proxy n edges of V_IntMod v -> v)
  U.mapM_ print result

--
-- Modular Arithmetic
--

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

--
-- instance U.Unbox (IntMod m)
--

newtype instance UM.MVector s (IntMod m) = MV_IntMod (UM.MVector s Int64)
newtype instance U.Vector (IntMod m) = V_IntMod (U.Vector Int64)

instance Data.Vector.Generic.Mutable.MVector UM.MVector (IntMod m) where -- needs MultiParamTypeClasses here
  basicLength (MV_IntMod mv) = Data.Vector.Generic.Mutable.basicLength mv
  basicUnsafeSlice i l (MV_IntMod mv) = MV_IntMod (Data.Vector.Generic.Mutable.basicUnsafeSlice i l mv)
  basicOverlaps (MV_IntMod mv) (MV_IntMod mv') = Data.Vector.Generic.Mutable.basicOverlaps mv mv'
  basicUnsafeNew l = MV_IntMod <$> Data.Vector.Generic.Mutable.basicUnsafeNew l
  basicInitialize (MV_IntMod mv) = Data.Vector.Generic.Mutable.basicInitialize mv
  basicUnsafeReplicate i x = MV_IntMod <$> Data.Vector.Generic.Mutable.basicUnsafeReplicate i (coerce x)
  basicUnsafeRead (MV_IntMod mv) i = coerce <$> Data.Vector.Generic.Mutable.basicUnsafeRead mv i
  basicUnsafeWrite (MV_IntMod mv) i x = Data.Vector.Generic.Mutable.basicUnsafeWrite mv i (coerce x)
  basicClear (MV_IntMod mv) = Data.Vector.Generic.Mutable.basicClear mv
  basicSet (MV_IntMod mv) x = Data.Vector.Generic.Mutable.basicSet mv (coerce x)
  basicUnsafeCopy (MV_IntMod mv) (MV_IntMod mv') = Data.Vector.Generic.Mutable.basicUnsafeCopy mv mv'
  basicUnsafeMove (MV_IntMod mv) (MV_IntMod mv') = Data.Vector.Generic.Mutable.basicUnsafeMove mv mv'
  basicUnsafeGrow (MV_IntMod mv) n = MV_IntMod <$> Data.Vector.Generic.Mutable.basicUnsafeGrow mv n

instance Data.Vector.Generic.Vector U.Vector (IntMod m) where -- needs MultiParamTypeClasses here
  basicUnsafeFreeze (MV_IntMod mv) = V_IntMod <$> Data.Vector.Generic.basicUnsafeFreeze mv
  basicUnsafeThaw (V_IntMod v) = MV_IntMod <$> Data.Vector.Generic.basicUnsafeThaw v
  basicLength (V_IntMod v) = Data.Vector.Generic.basicLength v
  basicUnsafeSlice i l (V_IntMod v) = V_IntMod (Data.Vector.Generic.basicUnsafeSlice i l v)
  basicUnsafeIndexM (V_IntMod v) i = coerce <$> Data.Vector.Generic.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_IntMod  mv) (V_IntMod v) = Data.Vector.Generic.basicUnsafeCopy mv v
  elemseq (V_IntMod v) x y = Data.Vector.Generic.elemseq v (coerce x) y

instance U.Unbox (IntMod m)
