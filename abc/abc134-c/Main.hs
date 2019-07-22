-- https://github.com/minoki/my-atcoder-solutions
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Char8 as BS

main = do
  n <- readLn
  xs <- U.replicateM n $ do
    Just (x, _) <- BS.readInt <$> BS.getLine
    return x
  let v = U.zipWith max (U.scanl' max 0 xs) (U.tail $ U.scanr' max 0 xs)
  U.forM_ v print
