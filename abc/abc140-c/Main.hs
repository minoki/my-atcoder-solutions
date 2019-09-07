-- https://github.com/minoki/my-atcoder-solutions
import Data.Char (isSpace)
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Char8 as BS

main = do
  n <- readLn
  xs <- U.unfoldrN (n-1) (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  print $ U.head xs + U.sum (U.zipWith min xs (U.tail xs)) + U.last xs
