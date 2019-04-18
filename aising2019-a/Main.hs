main = do
  n <- readLn
  h <- readLn
  w <- readLn
  print ((n - h + 1) * (n - w + 1) :: Int)
