{-# LANGUAGE BangPatterns #-}
import Data.List
import qualified Data.ByteString.Char8 as BS

main = do
  n <- readLn :: IO Int
  [a,b] <- map (read . BS.unpack) . BS.words <$> BS.getLine
  ps <- map (read . BS.unpack) . BS.words <$> BS.getLine
  let solve :: Int -> Int -> Int -> [Int] -> Int
      solve !na !nb !nc [] = min na (min nb nc)
      solve !na !nb !nc (x:xs)
        | x <= a = solve (na + 1) nb nc xs
        | x <= b = solve na (nb + 1) nc xs
        | otherwise = solve na nb (nc + 1) xs
  print $ solve 0 0 0 ps
{-
  let (p_a,p_bc) = partition (<= a) (ps :: [Int])
      (p_b,p_c) = partition (<= b) p_bc
      na = length p_a
      nb = length p_b
      nc = length p_c
  print $ min na (min nb nc)
-}
