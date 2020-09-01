-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}
import Data.Char (isSpace)
import Data.List
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.ST
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS
import qualified Test.QuickCheck as QC
import Data.Coerce

isPairwiseCoprime_naive :: Int -> U.Vector Int -> Bool
isPairwiseCoprime_naive _mbound xs = and [ gcd x y == 1 | x:ys <- tails (U.toList xs), y <- ys ]

toHistogram :: Int -> U.Vector Int -> U.Vector Int
toHistogram mbound xs = U.create $ do
  m <- UM.replicate (mbound + 1) (0 :: Int)
  U.forM_ xs $ \x -> do
    UM.modify m (+ 1) x
  return m

isPairwiseCoprime :: Int -> U.Vector Int -> Bool
isPairwiseCoprime !mbound !xs = maybe False (const True) $ runST $ runMaybeT $ do
  let !m = toHistogram mbound xs
  sieve <- UM.replicate (mbound + 1) True
  UM.write sieve 0 False
  UM.write sieve 1 False
  forM_ [2..mbound] $ \i -> do
    t <- UM.read sieve i
    when t $ do
      let loop !j !u
            | u >= 2 = mzero -- break
            | j > mbound = return ()
            | otherwise = do
                UM.write sieve j False
                loop (j + i) (u + m U.! j)
      loop (2 * i) (m U.! i)
  return () -- The answer is "Yes" (pairwise coprime)

isSetwiseCoprime_naive :: U.Vector Int -> Bool
isSetwiseCoprime_naive xs = U.foldl' gcd 0 xs == 1

isSetwiseCoprime :: U.Vector Int -> Bool
isSetwiseCoprime !xs = loop 0 0 == 1
  where
    loop !acc !i | i >= U.length xs = acc
                 | acc == 1 = acc
                 | otherwise = loop (gcd acc (xs U.! i)) (i + 1)

main = do
  n <- readLn @Int
  xs <- U.unfoldrN n (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let mbound = U.maximum xs
  if isPairwiseCoprime mbound xs then
    putStrLn "pairwise coprime"
  else if isSetwiseCoprime xs then
    putStrLn "setwise coprime"
  else
    putStrLn "not coprime"

prop_isPairwiseCoprime :: QC.NonEmptyList (QC.Positive Int) -> QC.Property
prop_isPairwiseCoprime xs' =
  let xs = U.fromList (coerce xs') :: U.Vector Int
      mbound = U.maximum xs
  in isPairwiseCoprime mbound xs QC.=== isPairwiseCoprime_naive mbound xs

prop_isSetwiseCoprime :: QC.NonEmptyList (QC.Positive Int) -> QC.Property
prop_isSetwiseCoprime xs' =
  let xs = U.fromList (coerce xs') :: U.Vector Int
  in isSetwiseCoprime xs QC.=== isSetwiseCoprime_naive xs

runTests :: IO ()
runTests = do
  QC.quickCheck $ QC.withMaxSuccess 500 $ QC.mapSize (* 1000) prop_isPairwiseCoprime
  QC.quickCheck $ QC.withMaxSuccess 500 $ QC.mapSize (* 1000) prop_isSetwiseCoprime
