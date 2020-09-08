{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}
module ModularArithmetic_TypeNats where
import Data.Int
import GHC.TypeNats (Nat, KnownNat, natVal, type (^), type (+))

--
-- Modular Arithmetic
--

-- type N = IntMod (10^9 + 7)

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

---

exEuclid :: (Eq a, Integral a) => a -> a -> (a, a, a)
exEuclid !f !g = loop 1 0 0 1 f g
  where loop !u0 !u1 !v0 !v1 !f 0 = (f, u0, v0)
        loop !u0 !u1 !v0 !v1 !f g =
          case divMod f g of
            (q,r) -> loop u1 (u0 - q * u1) v1 (v0 - q * v1) g r

instance KnownNat m => Fractional (IntMod m) where
  recip t@(IntMod x) = IntMod $ case exEuclid x modulus of
    (1,a,_) -> a `mod` modulus
    (-1,a,_) -> (-a) `mod` modulus
    _ -> error "not invertible"
    where modulus = fromIntegral (natVal t)
  fromRational = undefined
