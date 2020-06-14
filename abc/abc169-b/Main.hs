-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE TypeApplications #-}
import Data.Char (isSpace)
import Data.Int (Int64)
import Data.List (unfoldr)
import qualified Data.ByteString.Char8 as BS

main = do
  n <- readLn @Int
  xs <- unfoldr (BS.readInteger . BS.dropWhile isSpace) <$> BS.getLine
  let p = product xs
  let result | p > 10^18 = -1
             | otherwise = p
  print result
