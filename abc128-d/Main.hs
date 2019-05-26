import Data.Char
import Data.List
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Unboxed as U

main = do
  [n,k] <- map (read . BS.unpack) . BS.words <$> BS.getLine
  vs <- U.unfoldrN n (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let r = [ sum mm - sum (take l (takeWhile (< 0) mm))
          | i <- [0..min k n]
          , j <- [0..min (n - i) (k - i)]
          , let l = k - i - j
          , let mm = sort (U.toList (U.take i vs) ++ U.toList (U.drop (n - j) vs))
          ]
  print $ maximum r
