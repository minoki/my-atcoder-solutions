{-# LANGUAGE BangPatterns #-}
module Fermat where

powMod :: Int -> Int -> Int -> Int
powMod !_ 0 !_ = 1
powMod !a b !modulo = loop a a (b - 1)
  where
    loop :: Int -> Int -> Int -> Int
    loop !acc !a 0 = acc
    loop !acc !a 1 = (acc * a) `rem` modulo
    loop !acc !a i = case i `quotRem` 2 of
                       (j,0) -> loop acc ((a * a) `rem` modulo) j
                       (j,_) -> loop ((acc * a) `rem` modulo) ((a * a) `rem` modulo) j

fermatTest :: Int -> Int -> Bool
fermatTest n a = {- gcd n a == 1 && -} powMod a (n-1) n == 1
-- 2 <= a && a <= n-2

isPrime :: Int -> Bool
isPrime n | 14 <= n && n <= 10^6 = odd n && fermatTest n 2 && fermatTest n 3 && fermatTest n 5 && fermatTest n 7 && fermatTest n 11 && fermatTest n 13 && not isLiar
          | 0 <= n && n <= 13 = n `elem` [2,3,5,7,11,13]
          | otherwise = error "out of range"
  where isLiar = 162401 <= n && n <= 512461 && n `elem` [162401,252601,294409,334153,399001,410041,488881,512461]

main = do
  n <- readLn
  putStrLn $ if isPrime n
             then "YES"
             else "NO"
