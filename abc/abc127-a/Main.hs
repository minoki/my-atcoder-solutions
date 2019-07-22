{-# LANGUAGE ScopedTypeVariables #-}
main = do
  [a,b] :: [Int] <- map read . words <$> getLine
  print $ case () of
    _ | a <= 5 -> 0
      | a <= 12 -> b `quot` 2
      | otherwise -> b
