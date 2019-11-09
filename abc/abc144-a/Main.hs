-- https://github.com/minoki/my-atcoder-solutions
import Data.Char (isSpace)
import Data.List (unfoldr)
import qualified Data.ByteString.Char8 as BS

main = do
  [a,b] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  if a <= 9 && b <= 9 then
    print (a * b)
  else
    print (-1)
