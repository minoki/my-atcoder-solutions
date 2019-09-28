-- https://github.com/minoki/my-atcoder-solutions
import Data.Char (isSpace)
import Data.List (unfoldr)
import qualified Data.ByteString.Char8 as BS

main = do
  [n,k] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  xs <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  print $ length $ filter (>= k) xs
