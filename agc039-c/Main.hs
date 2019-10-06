-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Char (digitToInt, intToDigit)
import Data.Int (Int64)
import Data.Bits
import Data.Coerce
import Data.Foldable
import Data.Monoid
import Control.Monad
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS
import qualified Data.IntSet as IntSet
import qualified Data.IntMap.Lazy as IntMap
import Numeric

oneAction :: Int -> Int -> Int
oneAction !n x | even x = bit (n-1) + (x `shiftR` 1)
               | otherwise = x `shiftR` 1

naiveCount :: Int -> U.Vector Int
naiveCount !n = U.create $ do
  vec <- UM.replicate (2^n) (-1)
  let loop !m !set !i
        | i `IntSet.member` set = forM_ (IntSet.toList set) $ \j -> UM.write vec j m
        | otherwise = loop (m+1) (IntSet.insert i set) (oneAction n i)
  forM_ [0..2^n-1] $ \i -> do
    v <- UM.read vec i
    when (v == -1) $ do
      loop 0 IntSet.empty i
  return vec

naive :: Int -> Int -> Int
naive n x = U.sum $ U.take (x+1) $ naiveCount n

naiveS :: Int -> BS.ByteString -> Int
naiveS n x = naive n (readBinBS x)

---

readBinBS :: Num a => BS.ByteString -> a
readBinBS = BS.foldl' (\a c -> 2 * a + fromIntegral (digitToInt c)) 0

flipS :: BS.ByteString -> BS.ByteString
flipS = BS.map (\c -> if c == '0' then '1' else '0')

solve :: Int -> BS.ByteString -> N
solve !n !x = sum [ 2 * fromIntegral (n `quot` m * p) * a | (p,a) <- IntMap.toList d ]
  where
    countOne :: Int -> N
    countOne p = let q = n `quot` p
                     x0 = BS.take q x
                     y = BS.concat $ x0 : concat (replicate (p `quot` 2) [flipS x0, x0])
                 in readBinBS (BS.take q x) + (if y <= x then 1 else 0)
    factors :: [(Int, Int)]
    factors = filter (\(p,_) -> odd p) $ factor n
    cc :: IntMap.IntMap N
    cc = foldl' go (IntMap.singleton 1 (countOne 1)) factors
      where
        go t (!p,!k) = t `IntMap.union`
                       IntMap.fromList [ (a*p^i, countOne (a*p^i)) | (a,b) <- IntMap.toList t, i <- [1..k] ]
    m = product [ p^l | (p,l) <- factors ]
    mm = cc IntMap.! m
    d :: IntMap.IntMap N
    d = foldl' go (IntMap.singleton 1 mm) factors
      where
        go t (!p,!k) = t `IntMap.union`
                       IntMap.fromList [ (a*p^i, value)
                                       | (a,b) <- IntMap.toList t, i <- [1..k]
                                       , let value | a /= 1    = cc IntMap.! (m `div` (a*p^i)) - cc IntMap.! (m `div` a) - cc IntMap.! (m `div` (p^i)) + mm
                                                   | otherwise = cc IntMap.! (m `div` (a*p^i)) - cc IntMap.! (m `div` (p^(i-1)))
                                       ]

main = do
  n <- readLn :: IO Int
  x <- BS.getLine
  print $ solve n x
  -- print $ naiveS n x

showBin :: (Integral a, Show a) => a -> ShowS
showBin = showIntAtBase 2 intToDigit

showBinBS :: (Integral a, Show a) => a -> BS.ByteString
showBinBS x = BS.pack $ showBin x ""

padZeroBS :: Int -> BS.ByteString -> BS.ByteString
padZeroBS n s = BS.replicate (n - BS.length s) '0' <> s

checkBS :: Int -> BS.ByteString -> (Int, N)
checkBS n x = (naiveS n x, solve n x)

checkI :: Int -> Int -> (Int, N)
checkI n x = checkBS n $ padZeroBS n (showBinBS x)

checkBatch :: Int -> [(Int,Int,N)]
checkBatch n = let s = U.tail $ U.scanl' (+) 0 $ naiveCount n
               in [(x,y,z) | x <- [0..2^n-1], let y = s U.! x ; z = solve n (padZeroBS n (showBinBS x)), fromIntegral y /= z]

--
-- Modular Arithmetic
--

modulo :: Int64
modulo = 998244353
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

--
-- Sieve of Eratosthenes
--

infixr 5 !:
(!:) :: a -> [a] -> [a]
(!x) !: xs = x : xs

-- | エラトステネスの篩により、 max 以下の素数の一覧を構築して返す
-- >>> sieve 100
-- [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
sieve :: Int -> [Int]
sieve !max = 2 : U.ifoldr (\i isPrime xs -> if isPrime then (2 * i + 1) !: xs else xs) [] vec
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
primes :: [Int]
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
factor :: Int -> [(Int, Int)]
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
