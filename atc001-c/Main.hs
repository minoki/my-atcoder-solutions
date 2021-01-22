-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
import           Control.Exception           (assert)
import           Control.Monad
import           Data.Bits
import qualified Data.ByteString.Builder     as BSB
import qualified Data.ByteString.Char8       as BS
import           Data.Char                   (isSpace)
import           Data.Coerce
import           Data.Complex
import           Data.Int                    (Int64)
import           Data.List                   (unfoldr)
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           System.IO                   (stdout)

main = do
  n <- readLn @Int -- n <= 10^5
  (as,bs) <- fmap U.unzip $ U.replicateM n $ do
    [a,b] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    return (a,b)
  let p, q :: Poly U.Vector Int
      p = Poly $ normalizePoly (0 `U.cons` as)
      q = Poly $ normalizePoly (0 `U.cons` bs)
      -- v = coeffAsc (p * q)
      !v = coeffAsc p `mulFFT` coeffAsc q
      !l = U.length v
  BSB.hPutBuilder stdout $ mconcat
    [ if k < l then
        BSB.intDec (v U.! k) <> BSB.char8 '\n' -- <= 10^9
      else
        BSB.string8 "0\n"
    | k <- [1..2*n]
    ]

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

mulFFT :: U.Vector Int -> U.Vector Int -> U.Vector Int
mulFFT !f !g = let n' = U.length f + U.length g - 2
                   k = finiteBitSize n' - countLeadingZeros n'
                   !_ = assert (n' < 2^k) ()
                   n = bit k
                   u :: U.Vector (Complex Double)
                   u = U.generate n $ \j -> cis (fromIntegral j * (2 * pi / fromIntegral n))
                   us = iterate halve u
                   f' = U.generate n $ \j -> if j < U.length f then
                                               fromIntegral (f U.! j)
                                             else
                                               0
                   g' = U.generate n $ \j -> if j < U.length g then
                                               fromIntegral (g U.! j)
                                             else
                                               0
                   f'' = fft us f'
                   g'' = fft us g'
                   fg = U.generate n $ \j -> (f'' U.! j) * (g'' U.! j)
                   fg' = fft (map (U.map conjugate) us) fg
               in U.generate n $ \j -> round (realPart (fg' U.! j) / fromIntegral n)

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
