-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.List
import Data.Char
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Unboxed as U

readIntPair :: BS.ByteString -> (Int, Int)
readIntPair s = let Just (a, s') = BS.readInt s
                    Just (b, _) = BS.readInt $ BS.dropWhile isSpace s'
                in (a, b)

main = do
  (n,m) <- readIntPair <$> BS.getLine
  (ls,rs) <- fmap U.unzip $ U.replicateM m (readIntPair <$> BS.getLine)
  let maxL = U.maximum ls
      minR = U.minimum rs
  print $ max 0 (minR - maxL + 1)
