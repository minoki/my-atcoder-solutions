-- https://github.com/minoki/my-atcoder-solutions
import Data.Char (isSpace)
import Data.List (unfoldr)
import qualified Data.ByteString.Char8 as BS

main = do
  [a,b,x] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  if fromIntegral x <= (fromIntegral (a^2 * b) / 2 :: Double) then
    print $ (pi / 2 - atan (2 * fromIntegral x / fromIntegral (a * b^2))) / pi * 180
  else
    print $ atan (2 * fromIntegral (a^2 * b - x) / fromIntegral (a^3)) / pi * 180
