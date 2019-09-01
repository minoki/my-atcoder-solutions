-- https://github.com/minoki/my-atcoder-solutions
import Data.Char (isSpace)
import Data.List (unfoldr)
import qualified Data.ByteString.Char8 as BS

main = do
  [a,b] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  if b == 1
    then putStrLn "0"
    else print $ (b+a-3) `quot` (a-1)
