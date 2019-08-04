-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.Int (Int64)
import Data.List
import Control.Monad
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

main = do
  [n,k] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  xs <- U.unfoldrN n (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let s = U.sum xs
      ss = floor (sqrt (fromIntegral s)) :: Int
  let factors = sortBy (flip compare) $ concat [[i,s `quot` i] | i <- [1..ss], s `rem` i == 0]
  let try g = do
        let ys = mergeSort $ U.filter (/= (0,0)) $ U.map (\x -> let (q,r) = x `quotRem` g
                                                                in if r == 0
                                                                   then (0,0)
                                                                   else (r,g-r)
                                                         ) xs
        let zs0 = U.map (<= k) $ U.scanl' (+) 0 $ U.map fst ys
        let zs1 = U.map (<= k) $ U.scanr' (+) 0 $ U.map snd ys
        U.or $ U.zipWith (&&) zs0 zs1
  let result = head $ filter try factors
  print result
  {-
  if s - U.maximum xs <= k
    then print s
    else putStrLn "???"
  -}

--
-- Prime numbers
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

--
-- Merge Sort
--

mergeSortBy :: (U.Unbox a) => (a -> a -> Ordering) -> U.Vector a -> U.Vector a
mergeSortBy !cmp !vec = doSort vec
  where
    doSort vec | U.length vec <= 1 = vec
               | otherwise = let (xs, ys) = U.splitAt (U.length vec `quot` 2) vec
                             in merge (doSort xs) (doSort ys)
    merge xs ys = U.create $ do
      let !n = U.length xs
          !m = U.length ys
      result <- UM.new (n + m)
      let loop !i !j
            | i == n = U.copy (UM.drop (i + j) result) (U.drop j ys)
            | j == m = U.copy (UM.drop (i + j) result) (U.drop i xs)
            | otherwise = let !x = xs U.! i
                              !y = ys U.! j
                          in case cmp x y of
                               LT -> do UM.write result (i + j) x
                                        loop (i + 1) j
                               EQ -> do UM.write result (i + j) x
                                        UM.write result (i + j + 1) y
                                        loop (i + 1) (j + 1)
                               GT -> do UM.write result (i + j) y
                                        loop i (j + 1)
      loop 0 0
      return result

mergeSort :: (U.Unbox a, Ord a) => U.Vector a -> U.Vector a
mergeSort = mergeSortBy compare
