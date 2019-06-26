-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Control.Monad
import Data.Char (isSpace)
import Data.Int
import Data.List
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS
-- import Debug.Trace

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
  [n,x] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  -- n <= 200, x <= 10^5
  ss <- U.toList . mergeSortBy (\x y -> compare y x) . U.unfoldrN n (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  -- si <= 10^5
  let N result = solve x n ss 1
  print result

---

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
