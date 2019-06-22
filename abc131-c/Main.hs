-- https://github.com/minoki/my-atcoder-solutions
import Data.Char (isSpace)
import Data.List (unfoldr)
import qualified Data.ByteString.Char8 as BS

main = do
  [a,b,c,d] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let f n = n - n `quot` c - n `quot` d + n `quot` lcm c d
  print $ f b - f (a - 1)
