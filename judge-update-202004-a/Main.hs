-- https://github.com/minoki/my-atcoder-solutions
import Data.Char (isSpace)
import Data.List (unfoldr)
import qualified Data.ByteString.Char8 as BS

main = do
  [s,l,r] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  print $ max l (min r s)
