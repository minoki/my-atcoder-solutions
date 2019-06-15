-- https://github.com/minoki/my-atcoder-solutions
import Data.Char (isSpace)
import Data.List (unfoldr, tails)
import Control.Monad (replicateM)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map

main = do
  n <- readLn
  coords <- replicateM n $ do
    [x,y] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    return (x,y)
  let m = Map.fromListWith (+)
          [(c, 1) | (x,y):ts <- tails coords
                  , (x',y') <- ts
                  , let dx = x - x'
                        dy = y - y'
                        c | dx > 0 = (dx, dy)
                          | dx < 0 = (-dx, -dy)
                          | dy > 0 = (dx, dy)
                          | otherwise = (-dx, -dy)
                  ]
  let k | Map.null m = 0 -- n == 1
        | otherwise = maximum $ Map.elems m
  print $ n - k
