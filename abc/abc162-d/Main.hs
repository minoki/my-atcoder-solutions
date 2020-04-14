-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE TypeApplications #-}
import Data.Int (Int64)
import qualified Data.ByteString.Char8 as BS

main = do
  n <- readLn @Int
  s <- BS.getLine
  let n_r, n_g, n_b :: Int64
      n_r = fromIntegral (BS.count 'R' s)
      n_g = fromIntegral (BS.count 'G' s)
      n_b = fromIntegral (BS.count 'B' s)
  print $
    n_r * n_g * n_b
    - sum [ 1 :: Int64
          | d <- [1..n `div` 2]
          , i <- [0..n - 1 - 2 * d]
          , let j = i + d
                k = j + d -- k <= n - 1
          , s `BS.index` i /= s `BS.index` j
          , s `BS.index` i /= s `BS.index` k
          , s `BS.index` j /= s `BS.index` k
          ]
