-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
import Data.Int (Int64)
import Control.Monad
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

{-
-- i <= 4.5 * 10^18
integerSquareRoot :: Int64 -> Int64
integerSquareRoot i = loop 2 2121320343
  where loop !low !high | low == high = low
                        | otherwise = let !mid = (low + high) `quot` 2
                                          !mid2 = mid * mid
                                      in case compare i mid2 of
                                           LT -> loop low mid
                                           EQ -> mid
                                           GT -> loop mid high
-}
integerSquareRoot :: Int64 -> Int64
integerSquareRoot = round . sqrt . fromIntegral

solve :: [Int64] -> Int64 -> (Int64, Int64)
solve primes !n = case [(p,m) | p <- primes, (m,0) <- [n `quotRem` p]] of
                    [] -> error "No prime factor found"
                    (p,m):_ -> if m `rem` p == 0 then
                                 (p, m `quot` p)
                               else
                                 -- m <= 4.5 * 10^18
                                 (integerSquareRoot m, p)

main = do
  t <- readLn @Int
  tests <- U.replicateM t $ readLn @Int64
  let !primes = sieve 2080083 -- 2080083^3 < 9*10^18 < 2080084^3
  U.forM_ tests $ \t -> do
    let (p, q) = solve primes t
    putStrLn $ show p ++ " " ++ show q

--
-- Sieve of Eratosthenes
--

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
