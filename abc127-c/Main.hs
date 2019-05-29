-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Char
import qualified Data.ByteString.Char8 as BS

readIntPair :: BS.ByteString -> (Int, Int)
readIntPair s = let Just (a, s') = BS.readInt s
                    Just (b, _) = BS.readInt $ BS.dropWhile isSpace s'
                in (a, b)

main = do
  (n,m) <- readIntPair <$> BS.getLine
  let loop !maxL !minR 0 = print $ max 0 (minR - maxL + 1)
      loop !maxL !minR i = do
        (l,r) <- readIntPair <$> BS.getLine
        loop (max l maxL) (min r minR) (i - 1)
  loop minBound maxBound m
