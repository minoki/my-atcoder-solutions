-- https://github.com/minoki/my-atcoder-solutions
import Data.Char (isSpace)
import Data.List (unfoldr)
import qualified Data.ByteString.Char8 as BS

main = do
  [x,y] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  putStrLn $ if or [ 2 * 鶴 + 4 * 亀 == y | 鶴 <- [0..x], let 亀 = x - 鶴 ] then "Yes" else "No"
