main = do
  [h0,w0] <- map read . words <$> getLine
  [h1,w1] <- map read . words <$> getLine
  print ((h0 - h1) * (w0 - w1) :: Int)
