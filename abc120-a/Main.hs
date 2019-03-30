main = do
  [a,b,c] <- map read . words <$> getLine
  print $ (min c (b `quot` a) :: Int)
