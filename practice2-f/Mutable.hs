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
import           Control.Exception            (assert)
import           Control.Monad
import           Control.Monad.ST
import           Data.Bits
import qualified Data.ByteString.Builder      as BSB
import qualified Data.ByteString.Char8        as BS
import           Data.Char                    (isSpace)
import           Data.Int                     (Int64)
import           Data.List
import Data.Word
import qualified Data.Vector.Generic          as G
import qualified Data.Vector.Generic.Mutable  as GM
import qualified Data.Vector.Unboxing         as U
import qualified Data.Vector.Unboxing.Mutable as UM
import           GHC.TypeNats
import           System.IO                    (stdout)

-- main = print (findPrimitiveNthRoot (2^23) :: R)

main = do
  [n,m] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  xs <- U.map fromIntegral . U.unfoldrN n (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  ys <- U.map fromIntegral . U.unfoldrN m (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let !zs = U.take (n + m - 1) $ mulFFT xs ys
  BSB.hPutBuilder stdout $ mconcat (intersperse (BSB.char8 ' ') [ BSB.int64Dec x | R (IntMod x) <- U.toList zs ]) <> BSB.char8 '\n'

--
-- Fast Fourier Transform (FFT)
--

halve :: G.Vector vec a => vec a -> vec a
halve v = let n = G.length v
          in G.generate (n `quot` 2) $ \j -> v G.! (j * 2)

fftM :: forall vec a s. (Num a, G.Vector vec a)
     => [vec a] -- ^ For a primitive n-th root of unity @u@, @iterate halve [1,u,u^2 .. u^(n-1)]@
     -> G.Mutable vec s a -- ^ a vector of length n (= 2^k for some k)
     -> ST s ()
fftM (!u:u2) !f
  | n == 1 = return ()
  | otherwise = do let !n2 = n `quot` 2
                   forM_ [0..n2-1] $ \j -> do
                     !fj <- GM.read f j
                     !fj' <- GM.read f (j + n2)
                     GM.write f j $! fj + fj'
                     GM.write f (j + n2) $! (fj - fj') * u G.! j
                   let !(r0,r1') = GM.splitAt n2 f
                   fftM u2 r0
                   fftM u2 r1'
  where n = GM.length f
{-# SPECIALIZE fftM :: [U.Vector R] -> UM.MVector s R -> ST s () #-}

fft :: forall vec a. (Num a, G.Vector vec a)
    => [vec a] -- ^ For a primitive n-th root of unity @u@, @iterate halve [1,u,u^2 .. u^(n-1)]@
    -> vec a -- ^ a polynomial of length n (= 2^k for some k)
    -> vec a
fft us f = G.create $ do
  let !n = G.length f
  f' <- G.thaw f
  fftM us f'
  let !k = countTrailingZeros n
  forM_ [0..n-1] $ \i -> do
    let j = fromIntegral (bitRevN k (fromIntegral i))
    when (i < j) $ GM.swap f' i j
  return f'
{-# INLINE fft #-}

bitRevN :: Int -> Word -> Word
bitRevN w x = bitReverse x `shiftR` (finiteBitSize x - w)

bitReverse :: Word -> Word
bitReverse x = case finiteBitSize x of
                 32 -> fromIntegral (bitReverse32 (fromIntegral x))
                 64 -> fromIntegral (bitReverse64 (fromIntegral x))
                 _ -> error "bitReverse: unsupported word size"

bitReverse32 :: Word32 -> Word32
bitReverse32 !x0 = let !x1 = ((x0 .&. 0xaaaaaaaa) `shiftR` 1) .|. ((x0 .&. 0x55555555) `shiftL` 1)
                       !x2 = ((x1 .&. 0xcccccccc) `shiftR` 2) .|. ((x1 .&. 0x33333333) `shiftL` 2)
                       !x3 = ((x2 .&. 0xf0f0f0f0) `shiftR` 4) .|. ((x2 .&. 0x0f0f0f0f) `shiftL` 4)
                       !x4 = ((x3 .&. 0xff00ff00) `shiftR` 8) .|. ((x3 .&. 0x00ff00ff) `shiftL` 8)
                   in (x4 `shiftR` 16) .|. (x4 `shiftL` 16)

bitReverse64 :: Word64 -> Word64
bitReverse64 !x0 = let !x1 = ((x0 .&. 0xaaaaaaaaaaaaaaaa) `shiftR` 1) .|. ((x0 .&. 0x5555555555555555) `shiftL` 1)
                       !x2 = ((x1 .&. 0xcccccccccccccccc) `shiftR` 2) .|. ((x1 .&. 0x3333333333333333) `shiftL` 2)
                       !x3 = ((x2 .&. 0xf0f0f0f0f0f0f0f0) `shiftR` 4) .|. ((x2 .&. 0x0f0f0f0f0f0f0f0f) `shiftL` 4)
                       !x4 = ((x3 .&. 0xff00ff00ff00ff00) `shiftR` 8) .|. ((x3 .&. 0x00ff00ff00ff00ff) `shiftL` 8)
                       !x5 = ((x4 .&. 0xffff0000ffff0000) `shiftR` 16) .|. ((x4 .&. 0x0000ffff0000ffff) `shiftL` 16)
                   in (x5 `shiftR` 32) .|. (x5 `shiftL` 32)

zeroExtend :: (Num a, U.Unboxable a) => Int -> U.Vector a -> U.Vector a
zeroExtend n v | U.length v >= n = v
               | otherwise = U.create $ do
                   w <- UM.replicate n 0
                   U.copy (UM.take (U.length v) w) v
                   return w
{-# INLINE zeroExtend #-}

mulFFT :: forall a. (U.Unboxable a, Fractional a, PrimitiveRoot a) => U.Vector a -> U.Vector a -> U.Vector a
mulFFT !f !g = let n' = U.length f + U.length g - 2
                   k = finiteBitSize n' - countLeadingZeros n'
                   !_ = assert (n' < 2^k) ()
                   n = bit k
                   u0 = nthRoot n
                   us :: [U.Vector a]
                   us = iterate halve $ U.iterateN n (* u0) 1
                   f'' = fft us (zeroExtend n f)
                   g'' = fft us (zeroExtend n g)
                   v0 = recip u0
                   vs :: [U.Vector a]
                   vs = iterate halve $ U.iterateN n (* v0) 1
                   fg' = fft vs (U.zipWith (*) f'' g'')
                   !recip_n = recip (fromIntegral n)
               in U.map (* recip_n) fg'
{-# SPECIALIZE mulFFT :: U.Vector R -> U.Vector R -> U.Vector R #-}

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

-- Z / 998244353 Z
newtype R = R { unwrapR :: IntMod 998244353 } deriving newtype (Eq, Show, Num, Fractional)

instance U.Unboxable R where
  type Rep R = Int64

instance PrimitiveRoot R where
  nthRoot n | (998244353 - 1) `rem` n /= 0 = error "nthRoot: does not exist"
            | n .&. (n - 1) == 0 = let k = round (log (fromIntegral n) / log 2) :: Int
                                   in 31 ^ (2^(23 - k) :: Int)
            | otherwise = error "nthRoot: not implemented"

{-# RULES
"fromIntegral/Int->R" fromIntegral = R . fromIntegral_Int64_IntMod . (fromIntegral :: Int -> Int64)
"fromIntegral/Int64->R" fromIntegral = R . fromIntegral_Int64_IntMod
 #-}

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
  negate t@(IntMod x) | x == 0 = t
                      | otherwise = IntMod (modulus - x)
    where modulus = fromIntegral (natVal t)
  fromInteger n = let result = IntMod (fromInteger (n `mod` fromIntegral modulus))
                      modulus = natVal result
                  in result
  abs = undefined; signum = undefined
  {-# INLINE (+) #-}
  {-# INLINE (-) #-}
  {-# INLINE (*) #-}
  {-# INLINE negate #-}
  {-# INLINE fromInteger #-}
  {-# SPECIALIZE instance Num (IntMod 998244353) #-}

fromIntegral_Int64_IntMod :: KnownNat m => Int64 -> IntMod m
fromIntegral_Int64_IntMod n = result
  where
    result | 0 <= n && n < modulus = IntMod n
           | otherwise = IntMod (n `mod` modulus)
    modulus = fromIntegral (natVal result)
{-# SPECIALIZE fromIntegral_Int64_IntMod :: Int64 -> IntMod 998244353 #-}

{-# RULES
"fromIntegral/Int->IntMod" fromIntegral = fromIntegral_Int64_IntMod . (fromIntegral :: Int -> Int64) :: Int -> IntMod (10^9 + 7)
"fromIntegral/Int64->IntMod" fromIntegral = fromIntegral_Int64_IntMod :: Int64 -> IntMod (10^9 + 7)
"fromIntegral/Int->IntMod 998244353" fromIntegral = fromIntegral_Int64_IntMod . (fromIntegral :: Int -> Int64) :: Int -> IntMod 998244353
"fromIntegral/Int64->IntMod 998244353" fromIntegral = fromIntegral_Int64_IntMod :: Int64 -> IntMod 998244353
 #-}

instance U.Unboxable (IntMod m) where
  type Rep (IntMod m) = Int64

exEuclid :: (Eq a, Integral a) => a -> a -> (a, a, a)
exEuclid !f !g = loop 1 0 0 1 f g
  where loop !u0 !u1 !v0 !v1 !f 0 = (f, u0, v0)
        loop !u0 !u1 !v0 !v1 !f g =
          case divMod f g of
            (q,r) -> loop u1 (u0 - q * u1) v1 (v0 - q * v1) g r
{-# SPECIALIZE exEuclid :: Int64 -> Int64 -> (Int64, Int64, Int64) #-}

instance KnownNat m => Fractional (IntMod m) where
  recip t@(IntMod x) = IntMod $ case exEuclid x modulus of
    (1,a,_)  -> a `mod` modulus
    (-1,a,_) -> (-a) `mod` modulus
    _        -> error "not invertible"
    where modulus = fromIntegral (natVal t)
  fromRational = undefined
