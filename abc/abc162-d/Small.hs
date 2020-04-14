-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
import Data.Int (Int64)
import qualified Data.ByteString.Char8 as BS

main = do
  n <- readLn @Int
  s <- BS.getLine
  print $ sum [ 1 :: Int64
              | i <- [0..n-3]
              , j <- [i+1..n-2]
              , k <- [j+1..n-1]
              , s `BS.index` i /= s `BS.index` j
              , s `BS.index` i /= s `BS.index` k
              , s `BS.index` j /= s `BS.index` k
              , j - i /= k - j
              ]
