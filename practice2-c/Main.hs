-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE TypeApplications #-}
import           Control.Exception     (assert)
import           Control.Monad
import           Data.Bits
import qualified Data.ByteString.Char8 as BS
import           Data.Char             (isSpace)
import           Data.Int              (Int64)
import           Data.List             (unfoldr)
import           Data.Ratio
import qualified Test.QuickCheck       as QC

-- comb2 n = n * (n - 1) `quot` 2 without undue overflow
-- n even: comb2 n = (n `quot` 2) * (n - 1)
-- n odd: comb2 n = n * (n `quot` 2)
comb2 :: (Integral a, Bits a) => a -> a
comb2 n = (n `shiftR` 1) * ((n - 1) .|. 1)

-- floorSum n m a b
-- Assumptions:
-- * n: non-negative, m: positive
-- * a and b can be negative, or >= m
floorSum :: Int64 -> Int64 -> Int64 -> Int64 -> Int64
floorSum !n !m !a !b
  | assert (n >= 0 && m > 0) False = undefined
  | a < 0 = floorSum_positive 0 n m (- a) (b + a * (n - 1))
  | otherwise = floorSum_positive 0 n m a b
  where
    -- Invariants:
    -- * n: non-negative, m: positive, a: non-negative
    -- * 0 <= n <= m
    floorSum_positive :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64
    floorSum_positive !acc !n !m !a !b
      | n == 0 = acc
      | let m2 = m `quot` 2, a > m2 =
              let (q, a') = (a + m2) `quotRem` m
                  (a'',b'') = if a' < m2 then
                                (m2 - a', b - (m2 - a') * (n - 1))
                              else
                                (a' - m2, b)
              in floorSum_positive (acc + q * comb2 n) n m a'' b''
      | otherwise =
          let (q, b') = b `divMod` m
              t = (a * (n - 1) + b') `div` m
              -- t <= (m * (m - 1) + m) `div` m = m
          in floorSum_positive (acc + n * (q + t)) t a m (b' - m * t)

floorSum_naive :: Int64 -> Int64 -> Int64 -> Int64 -> Integer
floorSum_naive n m a b = sum [ floor ((toInteger a * toInteger i + toInteger b) % toInteger m) | i <- [0..n-1] ]

main = do
  t <- readLn @Int
  replicateM_ t $ do
    [n,m,a,b] <- map fromIntegral . unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    print $ floorSum n m a b

prop_comb2 :: Integer -> QC.Property
prop_comb2 n = comb2 n QC.=== n * (n - 1) `quot` 2

prop_floorSum :: QC.NonNegative (QC.Small Int64) -> QC.Positive Int64 -> Int64 -> Int64 -> QC.Property
prop_floorSum (QC.NonNegative (QC.Small n)) (QC.Positive m) a b = QC.within (100 * 1000) $ toInteger (floorSum n m a b) QC.=== floorSum_naive n m a b

prop_floorSum_r :: QC.Property
prop_floorSum_r = QC.forAllShrink (QC.choose (1, 10^4)) QC.shrink $ \n -> n >= 1 QC.==>
  QC.forAllShrink (QC.choose (1, 10^9)) QC.shrink $ \m -> m >= 1 QC.==>
  QC.forAllShrink (QC.choose (0, m - 1)) QC.shrink $ \a ->
  QC.forAllShrink (QC.choose (0, m - 1)) QC.shrink $ \b ->
  QC.within (100 * 1000 {- 100ms -}) $ toInteger (floorSum n m a b) QC.=== floorSum_naive n m a b
