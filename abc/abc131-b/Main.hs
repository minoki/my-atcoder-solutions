-- https://github.com/minoki/my-atcoder-solutions
import Data.Char (isSpace)
import Data.List (unfoldr, minimumBy)
import qualified Data.ByteString.Char8 as BS

main = do
  [n,l] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let xs = [l+i-1 | i <- [1..n]]
  print $ sum xs - minimumBy (\x y -> compare (abs x) (abs y)) xs
