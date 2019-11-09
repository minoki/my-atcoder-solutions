-- https://github.com/minoki/my-atcoder-solutions

main = do
  n <- readLn :: IO Int
  if or [ n == a * b | a <- [1..9], b <- [1..9] ] then
    putStrLn "Yes"
  else
    putStrLn "No"
