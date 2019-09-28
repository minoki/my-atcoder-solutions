-- https://github.com/minoki/my-atcoder-solutions
import Data.Char (isSpace)
import Data.List (unfoldr, sort)
import qualified Data.ByteString.Char8 as BS

main = do
  n <- readLn
  xs <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let result = map snd $ sort $ zip xs [1..n]
  putStrLn $ unwords $ map show result
