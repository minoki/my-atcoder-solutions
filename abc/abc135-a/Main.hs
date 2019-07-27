-- https://github.com/minoki/my-atcoder-solutions
import Data.Char (isSpace)
import Data.List (unfoldr)
import qualified Data.ByteString.Char8 as BS

main = do
  [a,b] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  if even (a - b)
    then print $ (a + b) `quot` 2
    else putStrLn "IMPOSSIBLE"
