{-# LANGUAGE ScopedTypeVariables #-}
main = do
  [r,d,x2000] :: [Int] <- map read . words <$> getLine
  mapM_ print $ take 10 $ tail $ iterate (\x -> r*x-d) x2000
