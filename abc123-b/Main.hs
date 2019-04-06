main = do
  a <- readLn :: IO Int
  b <- readLn
  c <- readLn
  d <- readLn
  e <- readLn
  let round10 :: Int -> Int
      round10 x = 10 * ((x + 9) `quot` 10)
      mod10' :: Int -> Int
      mod10' x | x `mod` 10 == 0 = 10
               | otherwise = x `mod` 10
      m = minimum $ map mod10' [a,b,c,d,e]
  print $ (sum $ map round10 [a,b,c,d,e]) - (10 - m)
