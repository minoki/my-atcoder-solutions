-- https://github.com/minoki/my-atcoder-solutions
import Data.Char (isSpace)
import Data.List (unfoldr, tails)
import Control.Monad
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Char8 as BS

main = do
  [n,d] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  points <- replicateM n $ do
    U.unfoldrN d (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  print $ sum [ 1
              | x:ys <- tails points
              , y <- ys
              , let distance = U.sum $ U.map (^2) (U.zipWith (-) x y)
              , distance == (floor $ sqrt $ fromIntegral distance)^2
              ]
