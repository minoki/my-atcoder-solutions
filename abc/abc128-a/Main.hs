import Data.Char
import Data.List
import qualified Data.ByteString.Char8 as BS

main = do
  [a,p] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  print $ (3 * a + p) `quot` 2
