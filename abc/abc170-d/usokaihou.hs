-- https://github.com/minoki/my-atcoder-solutions
-- 嘘解法につき注意
{-# LANGUAGE TypeApplications #-}
import Data.Char (isSpace)
import Data.List (unfoldr)
import Control.Monad
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS

main = do
  n <- readLn @Int
  xs <- U.unfoldrN n (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let m = U.maximum xs
  let vec :: U.Vector Int
      vec = U.create $ do
        vec <- UM.replicate (m+1) 0
        U.forM_ xs $ \x -> do
          UM.modify vec (+ 1) x
          forM_ [2*x,3*x..m] $ \i -> do
            UM.modify vec (+ 2) i
        return vec
  print $ length [ () | x <- U.toList xs, vec U.! x <= 1 ]
