import Data.Char
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Char8 as BS

readInt :: BS.ByteString -> Int
readInt s = case BS.readInt s of
              Just (x, _) -> x

main = do
  n <- readInt <$> BS.getLine
  -- xs <- U.fromListN . map read . words <$> getLine
  xs <- U.unfoldrN n (fmap (BS.dropWhile isSpace <$>) . BS.readInt) <$> BS.getLine
  let ls = U.scanl gcd 0 xs
      rs = U.scanr gcd 0 xs
  print $ U.maximum $ U.zipWith gcd ls (U.tail rs)
