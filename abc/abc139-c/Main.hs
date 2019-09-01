-- https://github.com/minoki/my-atcoder-solutions
import Data.Char (isSpace)
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Char8 as BS

main = do
  n <- readLn
  xs <- U.unfoldrN n (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let result :: Int
      result = U.maximum $ U.scanl' (\k (a,b) -> if a >= b then k + 1 else 0) 0 $ U.zip xs (U.tail xs)
  print result
