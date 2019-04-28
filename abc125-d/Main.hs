{-# LANGUAGE ScopedTypeVariables #-}
import Data.Char
import Data.Int
import Data.Bifunctor
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Char8 as BS

readInt :: BS.ByteString -> Int
readInt s = case BS.readInt s of
              Just (x, _) -> x

main = do
  n <- readInt <$> BS.getLine
  -- xs <- U.fromListN . map read . words <$> getLine
  xs :: U.Vector Int64 <- U.unfoldrN n (fmap (bimap fromIntegral (BS.dropWhile isSpace)) . BS.readInt) <$> BS.getLine
  let negatives = U.filter (< 0) xs
      abss = U.map abs xs
  if even (U.length negatives)
    then print $ U.sum abss
    else print $ U.sum abss - 2 * U.minimum abss
