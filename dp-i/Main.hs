{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Unboxed as U

-- n := V.length v + 1
-- v ! i = 表が i 枚、裏が n - i 枚出る確率
solve :: [Double] -> U.Vector Double
solve ps = go ps (U.singleton 1)
  where
    go [] !vec = vec
    go (!p:ps) !vec = go ps $ U.generate (U.length vec + 1) $ \i ->
      if i == 0
      then (1 - p) * vec U.! i
      else if i == U.length vec
           then p * vec U.! (i - 1)
           else p * vec U.! (i - 1) + (1 - p) * vec U.! i

main = do
  n :: Int <- readLn
  -- 1 <= n <= 2999, n is odd
  ps :: [Double] <- map (read . BS.unpack) . BS.words <$> BS.getLine
  let result = solve ps
  -- U.length result == n + 1
  print $ U.sum $ U.drop (n `quot` 2 + 1) result
