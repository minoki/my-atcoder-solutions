-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
import qualified Data.ByteString.Char8        as BS
import           Data.Char                    (isSpace)
import           Data.Coerce
import           Data.Int                     (Int64)
import qualified Data.Vector.Unboxing         as U

main = do
  n <- readLn @Int -- 1 <= n <= 10^5
  values <- U.map (fromIntegral :: Int -> N) . U.unfoldrN (n + 1) (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  t <- readLn @Int -- 1 <= t <= 10^9
  if t <= n then
    print (values U.! t)
  else do
    let t_n = product $ map fromIntegral [t-n..t] :: N
    let fact = U.scanl (*) 1 $ U.map fromIntegral $ U.fromListN n [1..n] :: U.Vector N
    print $ t_n * sum [ s * values U.! i / d
                      | i <- [0..n]
                      , let s = if even (n - i) then 1 else -1
                      , let d = fromIntegral (t - i) * fact U.! i * fact U.! (n - i)
                      ]

--
-- Modular Arithmetic
--

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

fromIntegral_Int64_N :: Int64 -> N
fromIntegral_Int64_N n | 0 <= n && n < modulo = N n
                       | otherwise = N (n `mod` modulo)

{-# RULES
"fromIntegral/Int->N" fromIntegral = fromIntegral_Int64_N . (fromIntegral :: Int -> Int64)
"fromIntegral/Int64->N" fromIntegral = fromIntegral_Int64_N
 #-}

---

exEuclid :: (Eq a, Integral a) => a -> a -> (a, a, a)
exEuclid !f !g = loop 1 0 0 1 f g
  where loop !u0 !u1 !v0 !v1 !f 0 = (f, u0, v0)
        loop !u0 !u1 !v0 !v1 !f g =
          case divMod f g of
            (q,r) -> loop u1 (u0 - q * u1) v1 (v0 - q * v1) g r

recipM :: Int64 -> Int64
recipM !x = case exEuclid x modulo of
             (1,a,_)  -> a `mod` modulo
             (-1,a,_) -> (-a) `mod` modulo
divM :: Int64 -> Int64 -> Int64
divM !x !y = x `mulMod` recipM y

instance Fractional N where
  (/) = coerce divM
  recip = coerce recipM
  fromRational = undefined

instance U.Unboxable N where
  type Rep N = Int64
