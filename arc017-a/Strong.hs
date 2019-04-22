{-# LANGUAGE BangPatterns #-}
module Strong where
import Data.Bits
import qualified Data.ByteString.Char8 as BS

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

strongTest :: Int -> Int -> Bool
strongTest 1 _ = False
strongTest n _ | even n = n == 2
strongTest n a = let d = gcd a n
                 in if d > 1
                    then False
                    else let v = countTrailingZeros (n-1)
                             m = (n-1) `shiftR` v
                             b0 = powMod a m n
                         in if b0 == 1
                            then True
                            else let loop !i !x
                                       | i == v = False
                                       | otherwise = let y = (x*x) `rem` n
                                                     in if y == 1
                                                        then let g = gcd (x + 1) n
                                                             in g == 1 || g == n
                                                        else loop (i+1) y
                                 in loop 0 b0

isPrime :: Int -> Bool
isPrime n | even n = n == 2
          | n == 3 = True
          | 0 <= n && n <= 10^6 = strongTest n 2 && (n < 2047 || strongTest n 3)
          | otherwise = error "out of range"

main = do
  -- n <- readLn
  Just (n, _) <- BS.readInt <$> BS.getLine
  if isPrime n
    then putStrLn "YES"
    else putStrLn "NO"
