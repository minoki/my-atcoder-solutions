import Data.Int (Int64)

main = do
  n <- readLn :: IO Int64
  a <- readLn
  b <- readLn
  c <- readLn
  d <- readLn
  e <- readLn
  let m = minimum [a,b,c,d,e]
  print $ 4 + (n + m - 1) `quot` m
