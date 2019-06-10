-- https://github.com/minoki/my-atcoder-solutions
import Data.Char (isSpace)
import Data.List (unfoldr)
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Char8 as BS

main = do
  n <- readLn
  destinations <- U.replicateM n $ do
    [t,x,y] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    return (t,x,y)
  let check (t1,x1,y1) (t2,x2,y2) = let dt = t2 - t1
                                        dx = abs (x1 - x2) + abs (y1 - y2)
                                    in dt >= dx && even (dt - dx)
  let result = U.all (uncurry check) $ U.zip (U.cons (0,0,0) destinations) destinations
  putStrLn $ if result then "Yes" else "No"
