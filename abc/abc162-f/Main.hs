-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.Int (Int64)
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Char8 as BS

main = do
  n <- readLn @Int
  -- 2 <= n <= 2*10^5
  xs <- U.map fromIntegral . U.unfoldrN n (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let loop :: Int -> Int64 -> Int64 -> Int64 -> (Int64, Int64, Int64)
      loop !k !a !b !c
        | k > n `quot` 2 = (a, b, c)
        | otherwise =
          -- k <= n `quot` 2
          let a' = a + xs U.! (2*k-2)
              b' = max a' (b + xs U.! (2*k-1))
              c' = if 2*k < n then max b' (c + xs U.! (2*k)) else b'
          in loop (k+1) a' b' c'
      (_,y,z) = loop 1 0 0 0
  print $ if even n then y else z
