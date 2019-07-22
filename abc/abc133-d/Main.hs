-- https://github.com/minoki/my-atcoder-solutions
import Data.Char (isSpace)
import Data.Int (Int64)
import Data.Bifunctor (first)
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Char8 as BS

main = do
  n <- readLn
  as <- U.unfoldrN n (readInt64 . BS.dropWhile isSpace) <$> BS.getLine
  let x0 = (U.sum as - 2 * sum [as U.! i | i <- [1,3..n-1]]) `quot` 2
  let xs = U.scanl' subtract x0 as
  putStrLn $ unwords $ map show $ U.toList $ U.map (* 2) $ U.init xs

readInt64 :: BS.ByteString -> Maybe (Int64, BS.ByteString)
readInt64 s = first fromIntegral <$> BS.readInt s
