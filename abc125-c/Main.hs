{-# LANGUAGE ScopedTypeVariables #-}
import Data.List
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Char8 as BS

content :: Int -> [Int] -> Int
content 1 _ = 1
content x [] = x
content x (y:ys) = content (gcd x y) ys

main = do
  n :: Int <- readLn
  xs :: U.Vector Int <- U.fromListN n . map (read . BS.unpack) . BS.words <$> BS.getLine
  let ls = U.scanl gcd 0 xs
      rs = U.scanr gcd 0 xs
  let gg = [gcd l r | i <- [0..n-1], let l = ls U.! i; r = rs U.! (i + 1)]
  print $ maximum gg
