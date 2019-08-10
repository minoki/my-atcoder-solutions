-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.Int (Int64)
import Data.List (unfoldr, intersperse)
import Control.Monad
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Generic
import qualified Data.Vector.Generic.Mutable
import qualified Data.ByteString.Char8 as BS
import Data.Proxy
import Data.Coerce
import Data.Monoid
import qualified Data.ByteString.Builder as BSB
import System.IO
import Data.Bits
---
import Unsafe.Coerce

type Poly p = U.Vector (IntMod p)

normalize :: IsInt64 p => Poly p -> Poly p
normalize p | U.null p || U.last p /= 0 = p
            | otherwise = normalize (U.init p)

addP :: IsInt64 p => Poly p -> Poly p -> Poly p
addP v w | n <= m = normalize $ U.accumulate (+) w (U.indexed v)
         | otherwise = normalize $ U.accumulate (+) v (U.indexed w)
  where n = U.length v
        m = U.length w

subP :: IsInt64 p => Poly p -> Poly p -> Poly p
subP v w | n <= m = normalize $ U.accumulate subtract w (U.indexed v)
         | otherwise = normalize $ U.accumulate (-) v (U.indexed w)
  where n = U.length v
        m = U.length w

naiveMulP :: IsInt64 p => Poly p -> Poly p -> Poly p
naiveMulP v w = U.generate (n + m - 1) $
           \i -> sum [(v U.! (i-j)) * (w U.! j) | j <- [max (i-n+1) 0..min i (m-1)]]
  where n = U.length v
        m = U.length w

mul1 :: IsInt64 p => IntMod p -> Poly p -> Poly p
-- mul1 k v = mulP (U.fromList [-k, 1]) v
mul1 k v = U.generate (U.length v + 1) $ \i -> if i == 0
                                               then -k * v U.! 0
                                               else if i == U.length v
                                                    then v U.! (i-1)
                                                    else v U.! (i-1) - k * (v U.! i)

doMulP :: forall p. IsInt64 p => Int -> Poly p -> Poly p -> Poly p
doMulP n !v !w | n <= 16 = naiveMulP v w
doMulP n !v !w
  | U.null v = v
  | U.null w = w
  | U.length v < n2 = let (w0, w1) = U.splitAt n2 w
                          u0 = doMulP n2 v w0
                          u1 = doMulP n2 v w1
                      in U.generate (U.length v + U.length w - 1)
                         $ \i -> case () of
                                   _ | i < n2     ->  u0 `at` i
                                     | i < n      -> (u0 `at` i) + (u1 `at` (i - n2))
                                     | i < n + n2 -> (u1 `at` (i - n2))
  | U.length w < n2 = let (v0, v1) = U.splitAt n2 v
                          u0 = doMulP n2 v0 w
                          u1 = doMulP n2 v1 w
                      in U.generate (U.length v + U.length w - 1)
                         $ \i -> case () of
                                   _ | i < n2     ->  u0 `at` i
                                     | i < n      -> (u0 `at` i) + (u1 `at` (i - n2))
                                     | i < n + n2 -> (u1 `at` (i - n2))
  | otherwise = let (v0, v1) = U.splitAt n2 v
                    (w0, w1) = U.splitAt n2 w
                    v0_1 = v0 `addP` v1
                    w0_1 = w0 `addP` w1
                    p = doMulP n2 v0_1 w0_1
                    q = doMulP n2 v0 w0
                    r = doMulP n2 v1 w1
                    -- s = (p `subP` q) `subP` r -- p - q - r
                    -- q + s*X^n2 + r*X^n
                in U.generate (U.length v + U.length w - 1)
                   $ \i -> case () of
                             _ | i < n2     ->   q `at` i
                               | i < n      -> ((q `at` i) + (p `at` (i - n2))) - ((q `at` (i - n2)) + (r `at` (i - n2)))
                               | i < n + n2 -> ((r `at` (i - n)) + (p `at` (i - n2))) - ((q `at` (i - n2)) + (r `at` (i - n2)))
                               | otherwise  ->   r `at` (i - n)
  where n2 = n `quot` 2
        at :: Poly p -> Int -> IntMod p
        at v i = if i < U.length v then v U.! i else 0

mulP :: IsInt64 p => Poly p -> Poly p -> Poly p
mulP !v !w = U.create $ do
  let !vl = U.length v
      !wl = U.length w
      n = ceiling ((log (fromIntegral (vl .|. wl)) :: Double) / log 2) :: Int
  U.thaw (doMulP (2^n) v w)

solve :: forall p. IsInt64 p => U.Vector Int -> Proxy p -> IO ()
solve a proxy = let p :: Int
                    p = fromIntegral (int64Val proxy)
                    xs :: V.Vector (Poly p)
                    xs = V.generate p $ \i -> U.fromList [fromIntegral (-i), 1]
                    ls, rs :: V.Vector (Poly p)
                    ls = V.scanl (\p k -> mul1 k p) (U.singleton 1) $ V.enumFromN 0 p
                    rs = V.scanr (\k p -> mul1 k p) (U.singleton 1) $ V.enumFromN 0 p
                    {-
                    ls = V.scanl' mulP (U.singleton 1) xs
                    rs = V.scanr' mulP (U.singleton 1) xs
                    -}
                    ps = V.zipWith mulP ls (V.tail rs)
                    aa = U.length $ U.filter (== 0) a
                    result | 2 * aa >= U.length a = U.map negate $ U.create $ do
                               vec <- UM.new p
                               V.forM_ (V.zip ps (V.convert a)) $ \(p,a) -> do
                                 when (a == 1) $ do
                                   U.imapM_ (\i x -> UM.modify vec (+ x) i) p
                               return vec
                               -- V.foldl' (\s (p,a) -> if a == 1 then s `addP` p else s) U.empty (V.zip ps (V.convert a))
                           | otherwise = U.singleton 1 `addP` (U.create $ do
                               vec <- UM.new p
                               V.forM_ (V.zip ps (V.convert a)) $ \(p,a) -> do
                                 when (a == 0) $ do
                                   U.imapM_ (\i x -> UM.modify vec (+ x) i) p
                               return vec
                                                              )
                in BSB.hPutBuilder stdout $ (mconcat $ intersperse (BSB.char7 ' ') $ map (BSB.int64Dec . getIntMod) $ U.toList (result <> U.replicate (p - U.length result) 0)) <> BSB.char7 '\n'

main = do
  p <- readLn -- 2 <= p <= 2999
  xs <- U.unfoldrN p (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  reifyInt64 (fromIntegral p) (solve xs)

---

newtype IntMod m = IntMod { getIntMod :: Int64 } deriving Eq
instance Show (IntMod m) where
  show (IntMod x) = show x
instance IsInt64 m => Num (IntMod m) where
  t@(IntMod x) + IntMod y = IntMod $ let !p = int64Val t
                                         !s = x + y
                                     in if s >= p then s - p else s
                                     -- ((x + y) `rem` int64Val t)
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
