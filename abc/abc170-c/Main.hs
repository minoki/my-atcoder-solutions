-- https://github.com/minoki/my-atcoder-solutions
import Data.Char (isSpace)
import Data.List (sort, unfoldr)
import qualified Data.ByteString.Char8 as BS

main = do
  [x,n] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  ps <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let (_, answer):_ = sort [ (abs (x - i), i) | i <- [0..101], i `notElem` ps ]
  print answer
