{-# LANGUAGE BangPatterns #-}
module Primes where
import Data.Int
import Control.Monad (forM_,when)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

infixr 5 !:
(!:) :: a -> [a] -> [a]
(!x) !: xs = x : xs

-- | エラトステネスの篩により、 max 以下の素数の一覧を構築して返す
-- >>> sieve 100
-- [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
sieve :: Int -> [Int64]
sieve !max = 2 : U.ifoldr (\i isPrime xs -> if isPrime then fromIntegral (2 * i + 1) !: xs else xs) [] vec
  where
    vec = U.create $ do
      vec <- UM.replicate ((max - 1) `quot` 2 + 1) True
      UM.write vec 0 False -- 1 is not a prime
      -- vec ! i : is (2 * i + 1) prime?
      let clear !p = forM_ [3*p,5*p..max] $ \n -> UM.write vec (n `quot` 2) False
          factorBound = floor (sqrt (fromIntegral max) :: Double)
          loop !i | 2 * i + 1 > factorBound = return ()
                  | otherwise = do b <- UM.read vec i
                                   when b $ clear (2 * i + 1)
                                   loop (i + 1)
      loop 1
      return vec

-- |
-- >>> takeWhile (< 100) primes
-- [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
primes :: [Int64]
primes = sieve 31622
-- floor (sqrt (10^9+9)) == 31622
-- length primes == 3401

-- x <= 10^9+9
-- |
-- >>> factor 100
-- [(2,2),(5,2)]
-- >>> factor 144
-- [(2,4),(3,2)]
-- >>> factor (10^9+6)
-- [(2,1),(500000003,1)]
-- >>> factor (10^9+7)
-- [(1000000007,1)]
factor :: Int64 -> [(Int64, Int)]
factor 0 = error "factor 0"
factor x | x > 10^9+9 = error "factor: too large"
factor x = loop x primes
  where
    loop 1 _ = []
    loop x (p:ps) = case factorOut 0 x p of
                      (0,y) -> loop x ps
                      (n,y) -> (p,n) : loop y ps
    loop x [] = [(x,1)]
    factorOut !n !x !p | (q,0) <- x `quotRem` p = factorOut (n+1) q p
                       | otherwise = (n, x)

-- $setup
-- >>> import Test.QuickCheck

-- |
-- prop> \(Positive (Small n)) -> n <= 10^9+9 ==> euler n == fromIntegral (length [x | x <- [1..n], gcd n x == 1])
-- >>> euler 324
-- 108
-- >>> euler 144
-- 48
euler :: Int64 -> Int64
euler !x = product [(p - 1) * p^(n-1) | (p,n) <- factor x]
