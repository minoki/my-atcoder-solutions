-- https://github.com/minoki/my-atcoder-solutions
import Data.Char (isSpace)
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Char8 as BS

main = do
  n <- readLn
  xs <- U.unfoldrN n (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  ys <- U.unfoldrN n (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  zs <- U.unfoldrN (n-1) (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let y = U.sum ys
  let z = U.sum $ U.zipWith (\i j -> if j == i + 1 then zs U.! (i-1) else 0) xs (U.tail xs)
  print $ y + z
