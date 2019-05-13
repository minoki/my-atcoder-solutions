{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
import Data.Int
import Data.List
import Control.Monad
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS

infixr 5 !:
(!:) :: a -> [a] -> [a]
(!x) !: xs = x : xs

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

primes :: [Int64]
primes = sieve (10^6)

factor :: Int64 -> [(Int64, Int)]
factor 0 = error "factor 0"
factor x = loop x primes
  where
    loop 1 _ = []
    loop x (p:ps) = case factorOut 0 x p of
                      (0,y) -> loop x ps
                      (n,y) -> (p,n) : loop y ps
    loop x [] = [(x,1)]
    factorOut !n !x !p | (q,0) <- x `quotRem` p = factorOut (n+1) q p
                       | otherwise = (n, x)

numOfDivisors :: [(Int64, Int)] -> Int64
numOfDivisors xs = product [fromIntegral a + 1 | (p,a) <- xs]
sumOfDivisors :: [(Int64, Int)] -> Int64
sumOfDivisors xs = product [(p^(a+1) - 1) `quot` (p - 1) | (p,a) <- xs]
divisors :: [(Int64, Int)] -> [Int64]
divisors xs = map product $ sequence $ [take (a+1) $ iterate (* p) 1 | (p,a) <- xs]

main = do
  n :: Int64 <- readLn
  let f = factor n
      ds = divisors f
  print $ sum [m | d <- ds, let m = d-1, m > 0, let q = n `quot` d, q < m]
