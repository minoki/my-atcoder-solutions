-- https://github.com/minoki/my-atcoder-solutions
import Data.Char (isSpace)
import Data.List (unfoldr)
import qualified Data.ByteString.Char8 as BS

main = do
  [k,x] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  putStrLn $ unwords $ map show [x-k+1..x+k-1]
