{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Control.Monad
import Data.Int
import Data.List
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

content :: U.Vector Int64 -> Int64
content v = loop 0 v
  where
    loop 1 _ = 1
    loop g v | U.null v = g
             | otherwise = loop (gcd (U.head v) g) (U.tail v)

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

-- (p,q) = splitCoprime x y
-- p * q == x, gcd p q == 1, gcd p y == 1
-- |
-- >>> splitCoprime 72 2
-- (9,8)
-- >>> splitCoprime 72 3
-- (8,9)
splitCoprime :: Int64 -> Int64 -> (Int64, Int64)
splitCoprime !x !y = loop x 1
  where
    loop !p !q = case gcd p y of
                   1 -> (p, q)
                   p' -> loop (p `quot` p') (q * p')

removeTailZeroes :: U.Vector Int64 -> U.Vector Int64
removeTailZeroes v | U.null v = v
                   | U.last v == 0 = removeTailZeroes (U.init v)
                   | otherwise = v

reduceP :: Int -> U.Vector Int64 -> U.Vector Int64
reduceP !p v | U.length v <= p-1 = removeTailZeroes $ U.map (`mod` fromIntegral p) v
             | otherwise = let n = U.length v
                           in removeTailZeroes $ U.generate (p-1) $ \i -> sum [v U.! (i + (p-1) * j) | j <- [0..(n-1-i) `quot` (p-1)]] `mod` fromIntegral p

valueAtMod :: U.Vector Int64 -> Int64 -> Int -> Int64
valueAtMod coeffs !x !p = U.foldl' (\a b -> (a * x + b) `mod` fromIntegral p) 0 coeffs

valueAt :: U.Vector Int64 -> Int64 -> Integer
valueAt coeffs !x = U.foldl' (\a b -> a * fromIntegral x + fromIntegral b) 0 coeffs

-- fromIntegral (valueAtOne f) = valueAt f 1
valueAtOne :: U.Vector Int64 -> Int64
valueAtOne coeffs = U.sum coeffs

-- fromIntegral (valueAtOne f) = valueAt f 1
valueAtMinusOne :: U.Vector Int64 -> Int64
valueAtMinusOne !coeffs = loop 0 0
  where
    loop !acc !i | i >= U.length coeffs = acc
                 | i + 1 >= U.length coeffs = acc + coeffs U.! i
                 | otherwise = loop (acc + coeffs U.! i - coeffs U.! (i + 1)) (i + 2)

test :: Int -> U.Vector Int64 -> Bool
test !p coeffs = let rr = reduceP p coeffs
                 in (U.head coeffs `mod` fromIntegral p == 0) && (U.null rr || all (\x -> valueAtMod rr x p == 0) [1..fromIntegral (p-1)])

-- |
-- >>> uniq [1,2,3,2,2]
-- [1,2,3,2]
uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq (x:xs) = loop x xs
  where
    loop !x (y:ys) | x == y = loop x ys
                   | otherwise = x : loop y ys
    loop !x [] = [x]

mergeSorted :: (Ord a) => [a] -> [a] -> [a]
mergeSorted [] ys = ys
mergeSorted xs [] = xs
mergeSorted xs@(x:xss) ys@(y:yss) = case compare x y of
  EQ -> x : mergeSorted xss yss
  LT -> x : mergeSorted xss ys
  GT -> y : mergeSorted xs yss

main = do
  n <- readLn -- 多項式の次数, <= 10^4
  coeffs :: U.Vector Int64 <- U.reverse <$> U.replicateM (n+1) readLn
  -- coeffs ! i <= 10^9
  -- 多項式は係数を昇順に持った Vector で表す（headが定数項）

  -- 多項式の内容（係数のGCD）の素因数は問題文の条件を満たす。
  let ct = content coeffs -- 多項式の内容
      primesCont = map (fromIntegral . fst) $ factor (abs ct) -- 内容を割り切る素数
      coeffsPP = U.map (`div` ct) coeffs -- 多項式の原始部分
  -- 以下、多項式の原始部分について考える。

  -- 問題文の条件を満たしうる素数の集合を絞り込みたい。
  -- 素朴に考えれば p は gcd(f(0), f(1), ..., f(n)) の素因数だが、絶対値が 2 以上の整数 x に対して f(x) を計算するのは現実的ではない。なので別の方法を考える。
  -- 問題文の条件を満たす素数は p | f(0) を満たすので、 f(0) /= 0 ならば f(0) の素因数を候補とすれば良い。
  -- そうでない場合、 Z/pZ において f は多項式として x(x-1)...(x-(p-1)) で割り切れるので、 f の次数は p 以上である。
  -- つまり n 以下の素数が p の候補となる（10^4 以下の素数は 1229 個ある）。
  let a0 = U.head coeffsPP
      a1 = gcd (valueAtOne coeffsPP) (valueAtMinusOne coeffsPP)
      candidates :: [Int64] -- 問題文の条件を満たす素数の候補
      candidates | a0 /= 0 = filter (\p -> ct `rem` fromIntegral p /= 0) $ map (fromIntegral . fst) $ factor (abs a0)
                 | 0 < a1 && a1 <= 10^9 = filter (\p -> ct `rem` fromIntegral p /= 0) $ map (fromIntegral . fst) $ factor a1
                 | otherwise = filter (\p -> ct `rem` fromIntegral p /= 0) $ takeWhile (<= fromIntegral n) primes
      primesPP = filter (\p -> test (fromIntegral p) coeffsPP) candidates

  mapM_ print $ mergeSorted primesCont primesPP
