{-# LANGUAGE BangPatterns #-}
import Control.Monad
import Data.Bits
import Data.Word
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS

infixr 5 !:
(!:) :: a -> [a] -> [a]
(!x) !: xs = x : xs

{-
type PrimeTable = U.Vector Word8

primeList :: PrimeTable -> [Int]
primeList !table = 2 : 3 : U.ifoldr (\i bb xs -> case bb of
                                         0x22 -> (6 * i + 1) !: (6 * i + 5) !: xs
                                         0x02 -> (6 * i + 1) !: xs
                                         0x20 -> (6 * i + 5) !: xs
                                         _ -> xs
                                     ) [] table

isPrime :: PrimeTable -> Int -> Bool
isPrime !table !n | n == 2 || n == 3 = True
isPrime !table !n = let (q,r) = n `quotRem` 6
                    in (r == 1 || r == 5) && testBit (table U.! q) r

genPrimeTable :: Int -> PrimeTable
genPrimeTable !max = U.create $ do
  -- vectorの最大のindex n に関して、 6 * n + 1 <= max < 6 * n + 7 となればOK
  let !maxIndex = (max - 1) `quot` 6
      max' = 6 * maxIndex + 5
  vec <- UM.replicate (maxIndex + 1) (0x22 :: Word8) -- 0b0010_0010
  UM.write vec 0 0x20 -- 0b0010_0000
  -- 6n+1, 6n+5
  let -- clear p : p の倍数を消す。ただし、 p の2倍、3倍はすでに消えているので、 p*(6n+1), p*(6n+5) の形の数を消す。
      clear !p = let !(!p5q,!p5r) = (5*p) `quotRem` 6
                     !(!p7q,!p7r) = (7*p) `quotRem` 6
                     clearLoop !n !nq !nq'
                       -- n == 6*nq + p5r
                       -- n + 2 * p == 6*nq' + p7r
                       | n+2*p > max' = if n > max'
                                        then return ()
                                        else UM.modify vec (\x -> clearBit x p5r) nq
                       | otherwise = do
                           -- n = 6*nq+p5r
                           UM.modify vec (\x -> clearBit x p5r) nq
                           -- n+2*p = 6*nq'+p7r
                           UM.modify vec (\x -> clearBit x p7r) nq'
                           clearLoop (n + 6 * p) (nq + p) (nq' + p)
                 in clearLoop (5 * p) p5q p7q
      factorBound = floor (sqrt (fromIntegral max) :: Double) -- max <= 2^53 を仮定する
      -- loop i n :
      loop !i !n
        -- n == 6 * i + 1
        | n > factorBound = return ()
        | otherwise = do bb <- UM.read vec i
                         when (testBit bb 1) $ clear n -- 6 * i + 1
                         when (testBit bb 5) $ clear (n + 4) -- 6 * i + 5
                         loop (i + 1) (n + 6)
  clear 5
  loop 1 7
  return vec
-}

type PrimeTable = U.Vector Bool

primeList :: PrimeTable -> [Int]
primeList table = 2 : U.ifoldr (\i isPrime xs -> if isPrime then (2 * i + 1) !: xs else xs) [] table

isPrime :: PrimeTable -> Int -> Bool
isPrime _ 2 = True
isPrime table n = let (q,r) = n `quotRem` 2
                  in r == 1 && table U.! q

genPrimeTable :: Int -> PrimeTable
genPrimeTable !max = U.create $ do
  -- vectorの最大のindex n に関して、 2 * n + 1 <= max < 2 * n + 3 となるように選ぶ
  -- 2 * len - 1 <= max < 2 * len + 1
  vec <- UM.replicate ((max - 1) `quot` 2 + 1) True
  UM.write vec 0 False -- 1 is not a prime
  let clear !p = forM_ [3*p,5*p..max] $ \n -> UM.write vec (n `quot` 2) False
      factorBound = floor (sqrt (fromIntegral max) :: Double) -- max <= 2^53 を仮定する
      loop !i | 2 * i + 1 > factorBound = return ()
              | otherwise = do b <- UM.read vec i
                               when b $ clear (2 * i + 1)
                               loop (i + 1)
  loop 1
  -- vec ! i is True if (2 * i + 1) is prime
  return vec

powMod :: Int -> Int -> Int -> Int
powMod !_ 0 !_ = 1
powMod !a b !modulo = loop a a (b - 1)
  where
    loop :: Int -> Int -> Int -> Int
    loop !acc !a 0 = acc
    loop !acc !a 1 = (acc * a) `rem` modulo
    loop !acc !a i = case i `quotRem` 2 of
                       (j,0) -> loop acc ((a * a) `rem` modulo) j
                       (j,_) -> loop ((acc * a) `rem` modulo) ((a * a) `rem` modulo) j

fermatTest :: Int -> Int -> Bool
fermatTest n a = powMod a (n-1) n == 1

solveEratos :: Int -> IO ()
solveEratos n = do
  let table = genPrimeTable (floor (sqrt (fromIntegral n) :: Double))
      primes = primeList table
  -- isPrime table n
  putStrLn $ if all (\p -> n `mod` p /= 0) primes
             then "YES"
             else "NO"

solve :: Int -> IO ()
solve n = do
  if True -- fermatTest n 2 && fermatTest n 3
    then do let table = genPrimeTable (floor (sqrt (fromIntegral n) :: Double))
                primes = primeList table
            -- isPrime table n
            putStrLn $ if all (\p -> n `mod` p /= 0) primes
                       then "YES"
                       else "NO"
    else putStrLn "NO"

main = do
  -- n <- readLn
  Just (n, _) <- BS.readInt <$> BS.getLine
  -- 17 <= n <= 10^6
  solve n
