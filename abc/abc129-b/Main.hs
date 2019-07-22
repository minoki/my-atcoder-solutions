-- https://github.com/minoki/my-atcoder-solutions
import Data.Char
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Char8 as BS

main = do
  n <- readLn
  ws <- U.unfoldrN n (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let wss = U.scanl' (+) 0 ws
      total = U.last wss
  print $ U.minimum $ U.map (\s -> abs (total - 2 * s)) wss
