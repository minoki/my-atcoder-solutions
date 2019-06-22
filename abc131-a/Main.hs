-- https://github.com/minoki/my-atcoder-solutions

main = do
  [a,b,c,d] <- getLine
  putStrLn $ if a == b || b == c || c == d
             then "Bad"
             else "Good"
