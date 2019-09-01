-- https://github.com/minoki/my-atcoder-solutions
import Data.Int (Int64)

main = do
  n <- readLn
  print (n * (n - 1) `quot` 2 :: Int64)
