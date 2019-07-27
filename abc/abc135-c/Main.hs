-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.Int (Int64)
import Data.List (unfoldr)
import qualified Data.ByteString.Char8 as BS

solve :: [Int64] -> [Int64] -> Int64
solve [x0,x1] [y0] = min (x0 + x1) y0
solve (x0:x1:xs) (y0:ys) = let !c = min (x0 + x1) y0
                               !d = if c <= x0
                                    then x1
                                    else x1 + x0 - c
                           in c + solve (d:xs) ys

main = do
  n <- readLn :: IO Int
  xs <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  ys <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  print $ solve (map fromIntegral xs) (map fromIntegral ys)
