import Data.Int
import Data.Bits
import Data.List

naiveSolution :: Int64 -> Int64 -> Int64
naiveSolution a b = foldl' xor 0 [a..b]

-- g n == scanl xor 0 [0..] !! n == naiveSolution 0 (n - 1)
g :: Int64 -> Int64
g 0 = 0
g x | odd x = x `xor` (((x + 1) `shiftR` 1) .&. 1)
    | even x = (x `shiftR` 1) .&. 1

solve :: Int64 -> Int64 -> Int64
solve a b = g (b + 1) `xor` g a

main = do
  [a,b] <- map read . words <$> getLine
  print $ solve a b
