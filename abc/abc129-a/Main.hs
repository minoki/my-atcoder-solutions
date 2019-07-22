-- https://github.com/minoki/my-atcoder-solutions
import Data.Char
import Data.List
import qualified Data.ByteString.Char8 as BS

main = do
  [p,q,r] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  print $ p + q + r - maximum [p, q, r]
