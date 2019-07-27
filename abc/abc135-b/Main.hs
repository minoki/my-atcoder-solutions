-- https://github.com/minoki/my-atcoder-solutions
import Data.Char (isSpace)
import Data.Int (Int64)
import Data.List (unfoldr, sort)
import qualified Data.ByteString.Char8 as BS

main = do
  n <- readLn :: IO Int
  xs <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let r = length $ filter id $ zipWith (/=) xs $ sort xs
  if r <= 2
    then putStrLn "YES"
    else putStrLn "NO"
