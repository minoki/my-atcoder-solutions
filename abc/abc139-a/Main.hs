-- https://github.com/minoki/my-atcoder-solutions

main = do
  s <- getLine
  t <- getLine
  print $ length $ filter id $ zipWith (==) s t
