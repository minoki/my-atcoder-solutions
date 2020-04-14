-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE TypeApplications #-}
import Data.Int (Int64)

main = do
  n <- readLn @Int64
  print $ sum [ x | x <- [1..n], x `rem` 3 /= 0, x `rem` 5 /= 0 ]
