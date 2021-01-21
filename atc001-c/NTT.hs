-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoStarIsType               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
import           Control.Exception            (assert)
import           Control.Monad
import           Data.Bits
import qualified Data.ByteString.Char8        as BS
import           Data.Char                    (isSpace)
import           Data.Coerce
import           Data.Complex
import           Data.Int                     (Int64)
import           Data.List                    (unfoldr)
import           Data.Proxy
import qualified Data.Vector.Generic          as G
import qualified Data.Vector.Unboxing         as U
import qualified Data.Vector.Unboxing.Mutable as UM
import           GHC.TypeNats                 (type (*), type (+), KnownNat,
                                               Nat, SomeNat (..), type (^),
                                               natVal, someNatVal)
import qualified Test.QuickCheck              as QC

main = do
  n <- readLn @Int -- n <= 10^5
  (as,bs) <- fmap U.unzip $ U.replicateM n $ do
    [a,b] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    -- a <= 100, b <= 100
    return (a,b)
  let p, q :: Poly U.Vector Int
      p = Poly $ normalizePoly (0 `U.cons` as)
      q = Poly $ normalizePoly (0 `U.cons` bs)
      -- v = coeffAsc (p * q)
      !v = coeffAsc p `mulFFTInt` coeffAsc q
      !l = U.length v
  forM_ [1..2*n] $ \k -> do
    print $ if k < l then
              v U.! k -- <= 10^9
            else
              0

--
-- Fast Fourier Transform (FFT)
--

halve :: G.Vector vec a => vec a -> vec a
halve v = let n = G.length v
          in G.generate (n `quot` 2) $ \j -> v G.! (j * 2)

fft :: forall vec a. (Num a, G.Vector vec a)
    => [vec a] -- ^ For a primitive n-th root of unity @u@, @iterate halve [1,u,u^2 .. u^(n-1)]@
    -> vec a -- ^ a polynomial of length n (= 2^k for some k)
    -> vec a
fft (u:u2) f | n == 1 = f
             | otherwise = let !n2 = n `quot` 2
                               r0, r1', t0, t1' :: vec a
                               r0 = G.generate n2 $ \j -> (f G.! j) + (f G.! (j + n2))
                               r1' = G.generate n2 $ \j -> ((f G.! j) - (f G.! (j + n2))) * u G.! j
                               !t0 = fft u2 r0
                               !t1' = fft u2 r1'
                           in G.generate n $ \j -> if even j then t0 G.! (j `quot` 2) else t1' G.! (j `quot` 2)
  where n = G.length f

mulFFT :: forall a. (U.Unboxable a, Fractional a, PrimitiveRoot a) => U.Vector a -> U.Vector a -> U.Vector a
mulFFT !f !g = let n' = U.length f + U.length g - 2
                   k = finiteBitSize n' - countLeadingZeros n'
                   !_ = assert (n' < 2^k) ()
                   n = bit k
                   u0 = nthRoot n
                   u :: U.Vector a
                   u = U.iterateN n (* u0) 1
                   us = iterate halve u
                   f' = U.generate n $ \j -> if j < U.length f then
                                               f U.! j
                                             else
                                               0
                   g' = U.generate n $ \j -> if j < U.length g then
                                               g U.! j
                                             else
                                               0
                   f'' = fft us f'
                   g'' = fft us g'
                   v0 = recip u0
                   v :: U.Vector a
                   v = U.iterateN n (* v0) 1
                   fg = U.generate n $ \j -> (f'' U.! j) * (g'' U.! j)
                   fg' = fft (iterate halve v) fg
               in U.generate n $ \j -> (fg' U.! j) / fromIntegral n

{-
mulFFTInt :: U.Vector Int -> U.Vector Int -> U.Vector Int64
mulFFTInt f g = let f1 = U.map fromIntegral f :: U.Vector R1
                    g1 = U.map fromIntegral g :: U.Vector R1
                    !h1 = mulFFT f1 g1
                    f2 = U.map fromIntegral f :: U.Vector R2
                    g2 = U.map fromIntegral g :: U.Vector R2
                    !h2 = mulFFT f2 g2
                in U.generate (U.length h1) $ \i -> case crt' (unwrapR1 $ h1 U.! i) (unwrapR2 $ h2 U.! i) of
                                                      IntMod x -> x
-}

mulFFTInt :: U.Vector Int -> U.Vector Int -> U.Vector Int64
mulFFTInt f g = let f' = U.map fromIntegral f :: U.Vector R5
                    g' = U.map fromIntegral g :: U.Vector R5
                in U.map (\(R5 (IntMod x)) -> x) (mulFFT f' g')

class PrimitiveRoot a where
  -- (nthRoot n)^n == 1
  -- (nthRoot (2 * m))^m == -1
  nthRoot :: Int -> a

order' :: (Eq a, Num a) => Int -> a -> Int
order' !m !x = go 1 x
  where
    go !n 1 = n
    go !n y | n > m = m + 1
    go !n y = go (n + 1) (x * y)

findPrimitiveNthRoot :: (Eq a, Num a) => Int -> a
findPrimitiveNthRoot n = head [ x | k <- [1..], let x = fromInteger k, order' n x == n ]

newtype R1 = R1 { unwrapR1 :: IntMod (5 * 2^25 + 1) } deriving newtype (Eq, Show, Num, Fractional, U.Unboxable)

instance PrimitiveRoot R1 where
  nthRoot n | (5 * 2^25) `rem` n /= 0 = error "nthRoot: does not exist"
            | n .&. (n - 1) == 0 = let k = round (log (fromIntegral n) / log 2) :: Int
                                   in 17 ^ (2^(25 - k) :: Int)
            | otherwise = error "nthRoot: not implemented"

newtype R2 = R2 { unwrapR2 :: IntMod (7 * 2^26 + 1) } deriving newtype (Eq, Show, Num, Fractional, U.Unboxable)

instance PrimitiveRoot R2 where
  nthRoot n | (7 * 2^26) `rem` n /= 0 = error "nthRoot: does not exist"
            | n .&. (n - 1) == 0 = let k = round (log (fromIntegral n) / log 2) :: Int
                                   in 30 ^ (2^(26 - k) :: Int)
            | otherwise = error "nthRoot: not implemented"

newtype R5 = R5 { unwrapR5 :: IntMod (483 * 2^21 + 1) } deriving newtype (Eq, Show, Num, Fractional, U.Unboxable)

instance PrimitiveRoot R5 where
  nthRoot n | (483 * 2^21) `rem` n /= 0 = error "nthRoot: does not exist"
            | n .&. (n - 1) == 0 = let k = round (log (fromIntegral n) / log 2) :: Int
                                   in 198 ^ (2^(21 - k) :: Int)
            | otherwise = error "nthRoot: not implemented"

{-# RULES
"fromIntegral/Int->R1" fromIntegral = R1 . fromIntegral_Int64_IntMod . (fromIntegral :: Int -> Int64)
"fromIntegral/Int64->R1" fromIntegral = R1 . fromIntegral_Int64_IntMod
"fromIntegral/Int->R2" fromIntegral = R2 . fromIntegral_Int64_IntMod . (fromIntegral :: Int -> Int64)
"fromIntegral/Int64->R2" fromIntegral = R2 . fromIntegral_Int64_IntMod
"fromIntegral/Int->R5" fromIntegral = R5 . fromIntegral_Int64_IntMod . (fromIntegral :: Int -> Int64)
"fromIntegral/Int64->R5" fromIntegral = R5 . fromIntegral_Int64_IntMod
 #-}

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
      n  -> Just n

    degree' :: G.Vector vec a => Poly vec a -> Int
    degree' (Poly xs) = case G.length xs of
      0 -> error "degree': zero polynomial"
      n -> n - 1

-- 組立除法
-- second constPoly (divModByDeg1 f t) = divMod f (Poly (G.fromList [-t, 1]))
divModByDeg1 :: (Eq a, Num a, G.Vector vec a) => Poly vec a -> a -> (Poly vec a, a)
divModByDeg1 f t = let w = G.postscanr (\a b -> a + b * t) 0 $ coeffAsc f
                   in (Poly (G.tail w), G.head w)

--
-- Modular Arithmetic
--

newtype IntMod (m :: Nat) = IntMod { unwrapN :: Int64 } deriving (Eq)

instance Show (IntMod m) where
  show (IntMod x) = show x

instance KnownNat m => Num (IntMod m) where
  t@(IntMod x) + IntMod y
    | x + y >= modulus = IntMod (x + y - modulus)
    | otherwise = IntMod (x + y)
    where modulus = fromIntegral (natVal t)
  t@(IntMod x) - IntMod y
    | x >= y = IntMod (x - y)
    | otherwise = IntMod (x - y + modulus)
    where modulus = fromIntegral (natVal t)
  t@(IntMod x) * IntMod y = IntMod ((x * y) `rem` modulus)
    where modulus = fromIntegral (natVal t)
  fromInteger n = let result = IntMod (fromInteger (n `mod` fromIntegral modulus))
                      modulus = natVal result
                  in result
  abs = undefined; signum = undefined

{-# RULES
"^9/Int" forall x. x ^ (9 :: Int) = let u = x; v = u * u * u in v * v * v
"^9/Integer" forall x. x ^ (9 :: Integer) = let u = x; v = u * u * u in v * v * v
 #-}

fromIntegral_Int64_IntMod :: KnownNat m => Int64 -> IntMod m
fromIntegral_Int64_IntMod n = result
  where
    result | 0 <= n && n < modulus = IntMod n
           | otherwise = IntMod (n `mod` modulus)
    modulus = fromIntegral (natVal result)

{-# RULES
"fromIntegral/Int->IntMod" fromIntegral = fromIntegral_Int64_IntMod . (fromIntegral :: Int -> Int64) :: Int -> IntMod (10^9 + 7)
"fromIntegral/Int64->IntMod" fromIntegral = fromIntegral_Int64_IntMod :: Int64 -> IntMod (10^9 + 7)
 #-}

instance U.Unboxable (IntMod m) where
  type Rep (IntMod m) = Int64

exEuclid :: (Eq a, Integral a) => a -> a -> (a, a, a)
exEuclid !f !g = loop 1 0 0 1 f g
  where loop !u0 !u1 !v0 !v1 !f 0 = (f, u0, v0)
        loop !u0 !u1 !v0 !v1 !f g =
          case divMod f g of
            (q,r) -> loop u1 (u0 - q * u1) v1 (v0 - q * v1) g r

instance KnownNat m => Fractional (IntMod m) where
  recip t@(IntMod x) = IntMod $ case exEuclid x modulus of
    (1,a,_)  -> a `mod` modulus
    (-1,a,_) -> (-a) `mod` modulus
    _        -> error "not invertible"
    where modulus = fromIntegral (natVal t)
  fromRational = undefined

recipM :: (Eq a, Integral a, Show a) => a -> a -> a
recipM !x modulo = case exEuclid x modulo of
                     (1,a,_) -> a `mod` modulo
                     (-1,a,_) -> (-a) `mod` modulo
                     (g,a,b) -> error $ show x ++ "^(-1) mod " ++ show modulo ++ " failed: gcd=" ++ show g

-- |
-- >>> crt 3 6 2 7
-- 9
-- >>> crt 2 5 3 9
-- 12
crt :: (Eq a, Integral a, Show a) => a -> a -> a -> a -> a
crt !a1 !m1 !a2 !m2 = let m1' = recipM m1 m2
                          m2' = recipM m2 m1
                          mm = m1 * m2
                      in
                        fromInteger ((toInteger m2 * toInteger m2' * toInteger a1 + toInteger m1 * toInteger m1' * toInteger a2) `mod` toInteger mm)
                        -- (m2 * m2' * a1 + m1 * m1' * a2) `mod` mm
                        -- ((m2 * m2' `mod` mm) * (a1 `mod` m1) `mod` mm + (m1 * m1' `mod` mm) * (a2 `mod` m2) `mod` mm) `mod` mm

crt' :: (KnownNat m1, KnownNat m2) => IntMod m1 -> IntMod m2 -> IntMod (m1 * m2)
crt' x1@(IntMod a1) x2@(IntMod !a2) = let m1, m2, m1', m2' :: Int64
                                          m1 = fromIntegral (natVal x1)
                                          m2 = fromIntegral (natVal x2)
                                          m1' = recipM m1 m2
                                          m2' = recipM m2 m1
                                      in IntMod (fromInteger ((toInteger m2 * toInteger m2' * toInteger a1 + toInteger m1 * toInteger m1' * toInteger a2) `mod` (toInteger m1 * toInteger m2)))

--
-- Tests
--

instance KnownNat m => QC.Arbitrary (IntMod m) where
  arbitrary = IntMod <$> QC.choose (0, m - 1)
    where
      m = fromIntegral (natVal (Proxy :: Proxy m))

runTests :: IO ()
runTests = do
  QC.quickCheck $ QC.forAll (QC.choose (2, 10^9+9)) $ \m x -> prop_recipM x (QC.Positive m)
  QC.quickCheck $ QC.forAll (QC.choose (2, 10^9+9)) $ \m1 -> QC.forAll (QC.choose (2, 10^9+9)) $ \m2 x y -> prop_crt x (QC.Positive m1) y (QC.Positive m2)
  QC.quickCheck $ QC.forAll (QC.choose (2, 10^9+9)) $ \m1 -> QC.forAll (QC.choose (2, 10^9+9)) $ \m2 x y -> prop_crt' x (QC.Positive m1) y (QC.Positive m2)

prop_recipM :: Int64 -> QC.Positive Int64 -> QC.Property
prop_recipM x (QC.Positive m) = gcd x m == 1 && m > 1 && m <= 10^9 + 9 QC.==>
  let y = recipM x m
  in 0 <= y QC..&&. y < m QC..&&. ((x `mod` m) * recipM x m) `mod` m QC.=== 1

prop_crt :: Int64 -> QC.Positive Int64 -> Int64 -> QC.Positive Int64 -> QC.Property
prop_crt a1 (QC.Positive m1) a2 (QC.Positive m2)
  = gcd m1 m2 == 1 QC.==> let r = crt a1 m1 a2 m2
                          in r `mod` m1 QC.=== a1 `mod` m1 QC..&&. r `mod` m2 QC.=== a2 `mod` m2

prop_crt' :: Int64 -> QC.Positive Int64 -> Int64 -> QC.Positive Int64 -> QC.Property
prop_crt' a1 (QC.Positive m1) a2 (QC.Positive m2)
  = gcd m1 m2 == 1 QC.==> case (someNatVal (fromIntegral m1), someNatVal (fromIntegral m2)) of
                            (SomeNat (Proxy :: Proxy m1), SomeNat (Proxy :: Proxy m2)) ->
                              let x = fromIntegral a1 :: IntMod m1
                                  y = fromIntegral a2 :: IntMod m2
                                  IntMod r = crt' x y
                              in r `mod` m1 QC.=== a1 `mod` m1 QC..&&. r `mod` m2 QC.=== a2 `mod` m2
