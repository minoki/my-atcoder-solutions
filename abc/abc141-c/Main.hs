-- https://github.com/minoki/my-atcoder-solutions
import Data.Char (isSpace)
import Data.List (unfoldr)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS

main = do
  [n,k,q] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  xs <- U.replicateM q $ do
    [a] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    return (a-1)
  let vec = U.create $ do
        vec <- UM.replicate n (k-q)
        U.forM_ xs $ \i -> do
          UM.modify vec (+ 1) i
        return vec
  U.forM_ vec $ \s -> do
    if s <= 0 then
      putStrLn "No"
    else
      putStrLn "Yes"
