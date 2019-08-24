-- https://github.com/minoki/my-atcoder-solutions
import Data.Char (isSpace)
import Data.List (unfoldr)
import qualified Data.ByteString.Char8 as BS

main = do
  [m,d] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  print $ length [(i,j) | i <- [1..m], j <- [10..d], let (d2,d1) = j `quotRem` 10, d1 >= 2, d2 >= 2, i == d1 * d2]
