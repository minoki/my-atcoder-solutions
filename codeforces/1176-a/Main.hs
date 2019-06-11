-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Int
import Control.Monad

solve :: Int64 -> Int
solve 0 = error "invalid input"
solve n = let (e2, n') = factorMultiplicity 2 n
              (e3, n'') = factorMultiplicity 3 n'
              (e5, n''') = factorMultiplicity 5 n''
          in if n''' == 1
             then e2 + 2 * e3 + 3 * e5
             else -1
  where
    factorMultiplicity :: Int64 -> Int64 -> (Int, Int64)
    factorMultiplicity !p = loop 0
      where loop !k !n = case n `quotRem` p of
                           (q,0) -> loop (k + 1) q
                           _ -> (k, n)

main = do
  q <- readLn
  replicateM_ q (solve <$> readLn >>= print)
