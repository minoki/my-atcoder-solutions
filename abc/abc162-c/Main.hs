-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE TypeApplications #-}

main = do
  k <- readLn @Int
  print $ sum [ if ab == 1 then k else sum [ gcd ab c | c <- [1..k] ]
              | a <- [1..k]
              , b <- [1..k]
              , let ab = gcd a b
              ]
