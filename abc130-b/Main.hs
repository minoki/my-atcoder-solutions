-- https://github.com/minoki/my-atcoder-solutions
import Data.Char (isSpace)
import Data.List (unfoldr)
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Char8 as BS

main = do
  [n,x] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  ys <- U.unfoldrN n(BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let zs = U.scanl' (+) 0 ys
  print $ U.length $ U.takeWhile (<= x) zs
