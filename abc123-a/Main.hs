main = do
  a <- readLn :: IO Int
  b <- readLn
  c <- readLn
  d <- readLn
  e <- readLn
  k <- readLn
  -- a < b < c < d < e
  if and [abs (x - y) <= k | x <- [a,b,c,d,e], y <- [a,b,c,d,e]]
    then putStrLn "Yay!"
    else putStrLn ":("
