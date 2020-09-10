-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE TypeApplications #-}
import           Control.Monad
import           Data.Bits
import qualified Data.ByteString.Char8 as BS
import           Data.Char             (isSpace)
import           Data.Int              (Int64)
import           Data.List             (unfoldr)
import           Data.Ratio
import qualified Test.QuickCheck       as QC
import Control.Exception (assert)

-- comb2 n = n * (n - 1) `quot` 2 without undue overflow
-- n even: comb2 n = (n `quot` 2) * (n - 1)
-- n odd: comb2 n = n * (n `quot` 2)
comb2 :: (Integral a, Bits a) => a -> a
comb2 n = (n `shiftR` 1) * ((n - 1) .|. 1)

-- floorSum n m a b
-- n: non-negative, m: positive
floorSum :: Int64 -> Int64 -> Int64 -> Int64 -> Int64
floorSum n m a b | assert (n >= 0 && m > 0) False = undefined
floorSum n m 0 b = n * floor (b % m)
floorSum 0 m a b = 0
floorSum n 1 a b = a * comb2 n + n * b
floorSum n m a b
  | a < 0 = floorSum n m (- a) (b + a * (n - 1))
{-
  | a >= m || a < 0 = case a `divMod` m of
                        (q, a') -> q * comb2 n + floorSum n m a' b
-}
  | let m2 = m `quot` 2
  , abs a > m2 = case (a + m2) `divMod` m of
                   (q, a') -> q * comb2 n + floorSum n m (a' - m2) b
  | b >= m || b < 0 = case b `divMod` m of
                        (q, b') -> q * n + floorSum n m a b'
  | n > m = case n `quotRem` m of
              (q, n') -> (q * n - comb2 (q + 1) * m) * a + q * floorSum m m a b + floorSum n' m a b
  | otherwise =
      -- 0 < a < m, 0 <= b < m, 0 < n <= m
      let t = floor ((toInteger a * (toInteger n - 1) + toInteger b) % toInteger m)
      in n * t + floorSum t a (- m) (b - m)

floorSum_naive :: Int64 -> Int64 -> Int64 -> Int64 -> Integer
floorSum_naive n m a b = sum [ floor ((fromIntegral a * fromIntegral i + fromIntegral b) % fromIntegral m) | i <- [0..n-1] ]

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
  QC.within (100 * 1000) $ toInteger (floorSum n m a b) QC.=== floorSum_naive n m a b

