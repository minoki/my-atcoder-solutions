-- https://github.com/minoki/my-atcoder-solutions

main = do
  s <- getLine
  let result = and $ zipWith elem s (cycle ["RUD","LUD"])
  putStrLn $ if result then "Yes" else "No"
