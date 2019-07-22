-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE ScopedTypeVariables #-}
import Data.Char (isSpace)
import Data.List (unfoldr)
import qualified Data.ByteString.Char8 as BS

main = do
  n :: Int <- readLn
  xs <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  print $ length $ filter id $ zipWith3 (\x y z -> min x z < y && y < max x z) xs (tail xs) (drop 2 xs)
