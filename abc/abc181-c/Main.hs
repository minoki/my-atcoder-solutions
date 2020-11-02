-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE TypeApplications #-}
import Data.Char (isSpace)
import Data.List
import Control.Monad
import qualified Data.ByteString.Char8 as BS

det2 a b c d = a * d - b * c

main = do
  n <- readLn @Int
  points <- replicateM n $ do
    [x,y] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    return (x,y)
  let result = or [ det2 (x1-x0) (y1-y0) (x2-x0) (y2-y0) == 0 | (x0,y0):ps <- tails points, (x1,y1):ps' <- tails ps, (x2,y2) <- ps' ]
  putStrLn $ if result then "Yes" else "No"
