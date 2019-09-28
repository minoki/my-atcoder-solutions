-- https://github.com/minoki/my-atcoder-solutions

main = do
  n <- readLn
  print (fromInteger ((n + 1) `quot` 2) / fromInteger n :: Double)
