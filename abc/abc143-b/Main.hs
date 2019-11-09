-- https://github.com/minoki/my-atcoder-solutions
import Data.Char (isSpace)
import Data.List (unfoldr, tails)
import qualified Data.ByteString.Char8 as BS

main = do
  n <- readLn :: IO Int
  d <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  print $ sum [ x * y | x:xs <- tails d, y <- xs ]
