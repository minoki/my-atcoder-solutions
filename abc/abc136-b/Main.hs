-- https://github.com/minoki/my-atcoder-solutions

main = do
  n <- readLn
  print $ length [() | i <- [1..n], odd $ length $ show (i :: Int)]
