-- https://github.com/minoki/my-atcoder-solutions
import Data.Char (isSpace)
import Data.List (unfoldr, sort)
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Char8 as BS

main = do
  n <- readLn
  xs <- U.fromListN n . sort . unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let (ys,zs) = U.splitAt (n `quot` 2) xs
      y = U.last ys
      z = U.head zs
  print $ z - y
