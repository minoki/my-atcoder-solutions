{-# LANGUAGE ScopedTypeVariables #-}
import Data.Int
import Data.List
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Char8 as BS

main = do
  n :: Int <- readLn
  xs :: U.Vector Int64 <- U.fromListN n . map (read . BS.unpack) . BS.words <$> BS.getLine
  let negatives = U.filter (<= 0) xs
  if even (U.length negatives)
    then print $ U.sum $ U.map abs xs
    else let positives = U.filter (> 0) xs
             mn = U.maximum negatives
             mp = U.minimum positives
         in if U.null positives || abs mp > abs mn
            then print $ (U.sum $ U.map abs xs) + 2 * mn
            else print $ (U.sum $ U.map abs xs) - 2 * mp
