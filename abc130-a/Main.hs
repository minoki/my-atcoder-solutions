-- https://github.com/minoki/my-atcoder-solutions
import Data.Char (isSpace)
import Data.List (unfoldr)
import qualified Data.ByteString.Char8 as BS

main = do
  [x,a] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  print $ if x < a then 0 else 10
