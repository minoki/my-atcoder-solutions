-- https://github.com/minoki/my-atcoder-solutions
import Data.Char (isSpace)
import Data.List (unfoldr)
import qualified Data.ByteString.Char8 as BS

main = do
  [l,r] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  print $ if r - l >= 2018
          then 0
          else minimum [ (i * j) `mod` 2019 | i <- [l..r-1], j <- [i+1..r] ]
