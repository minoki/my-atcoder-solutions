{-# LANGUAGE BangPatterns #-}
import Control.Monad
import Data.Int
import Data.List
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.ByteString.Char8 as BS
import Debug.Trace

newtype N = N Int64 deriving (Eq, Show)
modulo = 10^9+7 :: Int64
instance Num N where
  N x + N y = N ((x + y) `rem` modulo)
  N x - N y = N ((x - y) `mod` modulo)
  N x * N y = N ((x * y) `rem` modulo)
  fromInteger n = N (fromInteger (n `mod` fromIntegral modulo))
  abs = undefined; signum = undefined

solve :: Int -> Int -> [Int] -> N -> N
solve !x 0 [] !c = c * fromIntegral x
-- solve !x 1 [y] c bc = c * (traceShow (c,bc,x) $ fromIntegral (x `rem` y))
solve !x !n ss !c = sum $ do
  (k, t:ts) <- zip [0..] $ tails ss
  -- k + length ts + 1 == n
  -- k : t より大きいやつ
  return $ solve (x `rem` t) (n - k - 1) ts (product (map fromIntegral [n-1,n-2..n-k]) * c)
-- n == length ss

main = do
  [n,x] <- map (read . BS.unpack) . BS.words <$> BS.getLine
  -- n <= 200, x <= 10^5
  ss <- {- V.fromListN n . -} sortBy (\x y -> compare y x) . map (read . BS.unpack) . BS.words <$> BS.getLine
  -- si <= 10^5
  let N result = solve x n ss 1
  print result
