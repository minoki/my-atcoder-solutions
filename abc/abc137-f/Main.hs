-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
import Data.Char (isSpace)
import Data.Int (Int64)
import Data.List (intersperse)
import Control.Monad
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.ByteString.Char8 as BS
import Data.Proxy
import Data.Coerce
import Data.Monoid
import qualified Data.ByteString.Builder as BSB
import System.IO
---
import Unsafe.Coerce

solve :: forall p. IsInt64 p => U.Vector Int -> Proxy p -> IO ()
solve as proxy = let p :: Int
                     p = fromIntegral (int64Val proxy)
                     -- f = x^p - x
                     f :: Poly U.Vector (IntMod p)
                     f = Poly $ U.generate (p+1) $ \i -> if i == p then 1 else if i == 1 then -1 else 0
                     result = U.map negate $ U.create $ do
                       vec <- UM.replicate p 0
                       U.forM_ (U.indexed as) $ \(i,a) -> do
                         when (a == 1) $ do
                           -- let (p, 0) = f `divModPoly` Poly (U.fromList [fromIntegral (-i), 1])
                           let (p, 0) = f `divModByDeg1` fromIntegral i
                           U.imapM_ (\i x -> UM.modify vec (+ x) i) (coeffAsc p)
                       return vec
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

exEuclid :: (Eq a, Integral a) => a -> a -> (a, a, a)
exEuclid !f !g = loop 1 0 0 1 f g
  where loop !u0 !u1 !v0 !v1 !f 0 = (f, u0, v0)
        loop !u0 !u1 !v0 !v1 !f g =
          case divMod f g of
            (q,r) -> loop u1 (u0 - q * u1) v1 (v0 - q * v1) g r

instance IsInt64 p => Fractional (IntMod p) where
  recip (IntMod x) = let modulo = int64Val (Proxy :: Proxy p)
                     in IntMod $ case exEuclid x modulo of
                                   (1,a,_) -> a `mod` modulo
                                   (-1,a,_) -> (-a) `mod` modulo
  fromRational = undefined


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

instance GM.MVector UM.MVector (IntMod m) where -- needs MultiParamTypeClasses here
  basicLength (MV_IntMod mv) = GM.basicLength mv
  basicUnsafeSlice i l (MV_IntMod mv) = MV_IntMod (GM.basicUnsafeSlice i l mv)
  basicOverlaps (MV_IntMod mv) (MV_IntMod mv') = GM.basicOverlaps mv mv'
  basicUnsafeNew l = MV_IntMod <$> GM.basicUnsafeNew l
  basicInitialize (MV_IntMod mv) = GM.basicInitialize mv
  basicUnsafeReplicate i x = MV_IntMod <$> GM.basicUnsafeReplicate i (coerce x)
  basicUnsafeRead (MV_IntMod mv) i = coerce <$> GM.basicUnsafeRead mv i
  basicUnsafeWrite (MV_IntMod mv) i x = GM.basicUnsafeWrite mv i (coerce x)
  basicClear (MV_IntMod mv) = GM.basicClear mv
  basicSet (MV_IntMod mv) x = GM.basicSet mv (coerce x)
  basicUnsafeCopy (MV_IntMod mv) (MV_IntMod mv') = GM.basicUnsafeCopy mv mv'
  basicUnsafeMove (MV_IntMod mv) (MV_IntMod mv') = GM.basicUnsafeMove mv mv'
  basicUnsafeGrow (MV_IntMod mv) n = MV_IntMod <$> GM.basicUnsafeGrow mv n

instance G.Vector U.Vector (IntMod m) where -- needs MultiParamTypeClasses here
  basicUnsafeFreeze (MV_IntMod mv) = V_IntMod <$> G.basicUnsafeFreeze mv
  basicUnsafeThaw (V_IntMod v) = MV_IntMod <$> G.basicUnsafeThaw v
  basicLength (V_IntMod v) = G.basicLength v
  basicUnsafeSlice i l (V_IntMod v) = V_IntMod (G.basicUnsafeSlice i l v)
  basicUnsafeIndexM (V_IntMod v) i = coerce <$> G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_IntMod  mv) (V_IntMod v) = G.basicUnsafeCopy mv v
  elemseq (V_IntMod v) x y = G.elemseq v (coerce x) y

instance U.Unbox (IntMod m)

--
-- Univariate polynomial
--

newtype Poly vec a = Poly { coeffAsc :: vec a } deriving Eq

normalizePoly :: (Eq a, Num a, G.Vector vec a) => vec a -> vec a
normalizePoly v | G.null v || G.last v /= 0 = v
                | otherwise = normalizePoly (G.init v)

addPoly :: (Eq a, Num a, G.Vector vec a) => vec a -> vec a -> vec a
addPoly v w = case compare n m of
                LT -> G.generate m $ \i -> if i < n
                                           then v G.! i + w G.! i
                                           else w G.! i
                GT -> G.generate n $ \i -> if i < m
                                           then v G.! i + w G.! i
                                           else v G.! i
                EQ -> normalizePoly $ G.zipWith (+) v w
  where n = G.length v
        m = G.length w

subPoly :: (Eq a, Num a, G.Vector vec a) => vec a -> vec a -> vec a
subPoly v w = case compare n m of
                LT -> G.generate m $ \i -> if i < n
                                           then v G.! i - w G.! i
                                           else negate (w G.! i)
                GT -> G.generate n $ \i -> if i < m
                                           then v G.! i - w G.! i
                                           else v G.! i
                EQ -> normalizePoly $ G.zipWith (-) v w
  where n = G.length v
        m = G.length w

naiveMulPoly :: (Num a, G.Vector vec a) => vec a -> vec a -> vec a
naiveMulPoly v w = G.generate (n + m - 1) $
                   \i -> sum [(v G.! (i-j)) * (w G.! j) | j <- [max (i-n+1) 0..min i (m-1)]]
  where n = G.length v
        m = G.length w

doMulP :: (Eq a, Num a, G.Vector vec a) => Int -> vec a -> vec a -> vec a
doMulP n !v !w | n <= 16 = naiveMulPoly v w
doMulP n !v !w
  | G.null v = v
  | G.null w = w
  | G.length v < n2 = let (w0, w1) = G.splitAt n2 w
                          u0 = doMulP n2 v w0
                          u1 = doMulP n2 v w1
                      in G.generate (G.length v + G.length w - 1)
                         $ \i -> case () of
                                   _ | i < n2     ->  u0 `at` i
                                     | i < n      -> (u0 `at` i) + (u1 `at` (i - n2))
                                     | i < n + n2 -> (u1 `at` (i - n2))
  | G.length w < n2 = let (v0, v1) = G.splitAt n2 v
                          u0 = doMulP n2 v0 w
                          u1 = doMulP n2 v1 w
                      in G.generate (G.length v + G.length w - 1)
                         $ \i -> case () of
                                   _ | i < n2     ->  u0 `at` i
                                     | i < n      -> (u0 `at` i) + (u1 `at` (i - n2))
                                     | i < n + n2 -> (u1 `at` (i - n2))
  | otherwise = let (v0, v1) = G.splitAt n2 v
                    (w0, w1) = G.splitAt n2 w
                    v0_1 = v0 `addPoly` v1
                    w0_1 = w0 `addPoly` w1
                    p = doMulP n2 v0_1 w0_1
                    q = doMulP n2 v0 w0
                    r = doMulP n2 v1 w1
                    -- s = (p `subPoly` q) `subPoly` r -- p - q - r
                    -- q + s*X^n2 + r*X^n
                in G.generate (G.length v + G.length w - 1)
                   $ \i -> case () of
                             _ | i < n2     ->   q `at` i
                               | i < n      -> ((q `at` i) + (p `at` (i - n2))) - ((q `at` (i - n2)) + (r `at` (i - n2)))
                               | i < n + n2 -> ((r `at` (i - n)) + (p `at` (i - n2))) - ((q `at` (i - n2)) + (r `at` (i - n2)))
                               | otherwise  ->   r `at` (i - n)
  where n2 = n `quot` 2
        at :: (Num a, G.Vector vec a) => vec a -> Int -> a
        at v i = if i < G.length v then v G.! i else 0
{-# INLINE doMulP #-}

mulPoly :: (Eq a, Num a, G.Vector vec a) => vec a -> vec a -> vec a
mulPoly !v !w = let k = ceiling ((log (fromIntegral (max n m)) :: Double) / log 2) :: Int
                in doMulP (2^k) v w
  where n = G.length v
        m = G.length w
{-# INLINE mulPoly #-}

zeroPoly :: (G.Vector vec a) => Poly vec a
zeroPoly = Poly G.empty

constPoly :: (Eq a, Num a, G.Vector vec a) => a -> Poly vec a
constPoly 0 = Poly G.empty
constPoly x = Poly (G.singleton x)

scalePoly :: (Eq a, Num a, G.Vector vec a) => a -> Poly vec a -> Poly vec a
scalePoly a (Poly xs)
  | a == 0 = zeroPoly
  | otherwise = Poly $ G.map (* a) xs

valueAtPoly :: (Num a, G.Vector vec a) => Poly vec a -> a -> a
valueAtPoly (Poly xs) t = G.foldr' (\a b -> a + t * b) 0 xs

instance (Eq a, Num a, G.Vector vec a) => Num (Poly vec a) where
  (+) = coerce (addPoly :: vec a -> vec a -> vec a)
  (-) = coerce (subPoly :: vec a -> vec a -> vec a)
  negate (Poly v) = Poly (G.map negate v)
  (*) = coerce (mulPoly :: vec a -> vec a -> vec a)
  fromInteger = constPoly . fromInteger
  abs = undefined; signum = undefined

divModPoly :: (Eq a, Fractional a, G.Vector vec a) => Poly vec a -> Poly vec a -> (Poly vec a, Poly vec a)
divModPoly f g@(Poly w)
  | G.null w = error "divModPoly: divide by zero"
  | degree f < degree g = (zeroPoly, f)
  | otherwise = loop zeroPoly (scalePoly (recip b) f)
  where
    g' = toMonic g
    b = leadingCoefficient g
    -- invariant: f == q * g + scalePoly b p
    loop q p | degree p < degree g = (q, scalePoly b p)
             | otherwise = let q' = Poly (G.drop (degree' g) (coeffAsc p))
                           in loop (q + q') (p - q' * g')

    toMonic :: (Fractional a, G.Vector vec a) => Poly vec a -> Poly vec a
    toMonic f@(Poly xs)
      | G.null xs = zeroPoly
      | otherwise = Poly $ G.map (* recip (leadingCoefficient f)) xs

    leadingCoefficient :: (Num a, G.Vector vec a) => Poly vec a -> a
    leadingCoefficient (Poly xs)
      | G.null xs = 0
      | otherwise = G.last xs

    degree :: G.Vector vec a => Poly vec a -> Maybe Int
    degree (Poly xs) = case G.length xs - 1 of
      -1 -> Nothing
      n -> Just n

    degree' :: G.Vector vec a => Poly vec a -> Int
    degree' (Poly xs) = case G.length xs of
      0 -> error "degree': zero polynomial"
      n -> n - 1

-- second constPoly (divModByDeg1 f t) = divMod f (Poly (G.fromList [-t, 1]))
divModByDeg1 :: (Eq a, Num a, G.Vector vec a) => Poly vec a -> a -> (Poly vec a, a)
divModByDeg1 f t = let w = G.postscanr (\a b -> a + b * t) 0 $ coeffAsc f
                   in (Poly (G.tail w), G.head w)
