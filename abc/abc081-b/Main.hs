import Data.List
import Data.Bits
import qualified Data.ByteString.Char8 as BS

readInt s = case BS.readInt s of
              Just (x, _) -> x

main = do
  _n <- readInt <$> BS.getLine
  a <- foldl' (.|.) 0 . map readInt . BS.words <$> BS.getLine
  print $ countTrailingZeros a
