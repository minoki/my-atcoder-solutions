{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}
module ModularArithmetic_TypeNats where
import Data.Int
import GHC.TypeNats
import qualified Test.QuickCheck as QC
import Data.Proxy
import Control.Exception (assert)

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

{-
import qualified Data.Vector.Unboxing  as U
instance U.Unboxable (IntMod m) where
  type Rep (IntMod m) = Int64
-}

recipM :: (Eq a, Integral a, Show a) => a -> a -> a
recipM !x modulo = case exEuclid x modulo of
                     (1,a,_) -> a `mod` modulo
                     (-1,a,_) -> (-a) `mod` modulo
                     (g,a,b) -> error $ show x ++ "^(-1) mod " ++ show modulo ++ " failed: gcd=" ++ show g

-- |
-- Assumption: @gcd m1 m2 == 1@
--
-- >>> crt 3 6 2 7
-- 9
-- >>> crt 2 5 3 9
-- 12
crt :: (Eq a, Integral a, Show a) => a -> a -> a -> a -> a
crt !a1 !m1 !a2 !m2 = let !(s1,s2) = case exEuclid m2 m1 of
                                       (1,b,c) -> (b `mod` m1, c `mod` m2)
                                       (-1,b,c) -> ((-b) `mod` m1, (-c) `mod` m2)
                                       (g,a,b) -> error $ "CRT: " ++ show m1 ++ " and " ++ show m2 ++ " not coprime; gcd=" ++ show (abs g)
                          !_ = assert (s1 == recipM m2 m1) ()
                          !_ = assert (s2 == recipM m1 m2) ()
                          c1 = ((a1 `mod` m1) * s1) `rem` m1
                          c2 = ((a2 `mod` m2) * s2) `rem` m2
                          m = m1 * m2
                          result = c1 * m2 + c2 * m1
                      in if result < m then
                           result
                         else
                           result - m
{-# SPECIALIZE crt :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 #-}

-- |
-- Assumption: @gcd m1 m2 == 1@
crt' :: (KnownNat m1, KnownNat m2) => IntMod m1 -> IntMod m2 -> IntMod (m1 * m2)
crt' x1@(IntMod !a1) x2@(IntMod !a2) = let !(s1,s2) = case exEuclid m2 m1 of
                                             (1,b,c) -> (b `mod` m1, c `mod` m2)
                                             (-1,b,c) -> ((-b) `mod` m1, (-c) `mod` m2)
                                             (g,a,b) -> error $ "CRT: " ++ show m1 ++ " and " ++ show m2 ++ " not coprime; gcd=" ++ show (abs g)
                                           !_ = assert (s1 == recipM m2 m1) ()
                                           !_ = assert (s2 == recipM m1 m2) ()
                                           c1 = (a1 * s1) `rem` m1
                                           c2 = (a2 * s2) `rem` m2
                                           result = c1 * m2 + c2 * m1
                                       in IntMod $ if result < m then
                                                     result
                                                   else
                                                     result - m
  where
    m1 = fromIntegral (natVal x1)
    m2 = fromIntegral (natVal x2)
    m = m1 * m2

--
-- Tests
--

instance KnownNat m => QC.Arbitrary (IntMod m) where
  arbitrary = let result = IntMod <$> QC.choose (0, m - 1)
                  m = fromIntegral (natVal (proxy result))
              in result
    where
      proxy :: f (IntMod m) -> Proxy m
      proxy _ = Proxy

runTests :: IO ()
runTests = do
  QC.quickCheck $ QC.forAll (QC.choose (2, 10^9+9)) $ \m x -> prop_recipM x (QC.Positive m)
  QC.quickCheck $ QC.withMaxSuccess 10000 $ QC.forAll (QC.choose (2, 10^9+9)) $ \m1 -> QC.forAll (QC.choose (2, 10^9+9)) $ \m2 x y -> prop_crt x (QC.Positive m1) y (QC.Positive m2)
  QC.quickCheck $ QC.withMaxSuccess 10000 $ QC.forAll (QC.choose (2, 10^9+9)) $ \m1 -> QC.forAll (QC.choose (2, 10^9+9)) $ \m2 x y -> prop_crt' x (QC.Positive m1) y (QC.Positive m2)

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
                            (SomeNat p1, SomeNat p2) ->
                              let x = fromIntegral a1 `asIntModProxy` p1
                                  y = fromIntegral a2 `asIntModProxy` p2
                                  IntMod r = crt' x y
                              in 0 <= r QC..&&. r < m1 * m2 QC..&&. r `mod` m1 QC.=== a1 `mod` m1 QC..&&. r `mod` m2 QC.=== a2 `mod` m2
  where
    asIntModProxy :: IntMod m -> Proxy m -> IntMod m
    asIntModProxy x _ = x
