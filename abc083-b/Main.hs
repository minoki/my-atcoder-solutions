-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.List (unfoldr)
import qualified Data.ByteString.Char8 as BS

main = do
  [n,a,b] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let sumDigits a = loop 0 a
        where loop !acc 0 = acc
              loop !acc a = case a `quotRem` 10 of
                              (q,r) -> loop (acc + r) q
  print $ sum [x | x <- [1..n], let s = sumDigits x, a <= s && s <= b]
