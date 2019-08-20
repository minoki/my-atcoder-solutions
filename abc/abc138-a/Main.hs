-- https://github.com/minoki/my-atcoder-solutions

main = do
  a <- readLn :: IO Int
  s <- getLine
  if a >= 3200 then
    putStrLn s
  else
    putStrLn "red"
