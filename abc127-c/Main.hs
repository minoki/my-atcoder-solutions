{-# LANGUAGE BangPatterns #-}
import Data.List
import Data.Char
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Unboxed as U

main = do
  [n,m] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  (ls,rs) <- fmap U.unzip $ U.replicateM m $ do
    [l,r] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    return (l,r)
  let maxL = U.maximum ls
      minR = U.minimum rs
  print $ max 0 (minR - maxL + 1)
