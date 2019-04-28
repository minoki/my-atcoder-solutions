import Data.Int
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Char8 as BS

readInt :: BS.ByteString -> Int
readInt s = case BS.readInt s of
              Just (x, _) -> x

readInt64 :: BS.ByteString -> Int64
readInt64 s = case BS.readInteger s of
              Just (x, _) -> fromInteger x

main = do
  n <- readInt <$> BS.getLine
  xs <- U.fromListN n . map readInt64 . BS.words <$> BS.getLine
  let negatives = U.filter (<= 0) xs
  if even (U.length negatives)
    then print $ U.sum $ U.map abs xs
    else let positives = U.filter (> 0) xs
             mn = U.maximum negatives
             mp = U.minimum positives
         in if U.null positives || abs mp > abs mn
            then print $ (U.sum $ U.map abs xs) + 2 * mn
            else print $ (U.sum $ U.map abs xs) - 2 * mp
