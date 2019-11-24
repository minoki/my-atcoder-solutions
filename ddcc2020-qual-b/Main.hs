-- https://github.com/minoki/my-atcoder-solutions
import Data.Char (isSpace)
import Data.List (unfoldr)
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Char8 as BS

main = do
  n <- readLn :: IO Int
  xs <- U.unfoldrN n (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let ys = U.scanl (+) 0 xs
  let total = U.last ys
  let (zs,ws) = U.span (\x -> 2 * x <= total) ys
  if 2 * (U.last zs) == total then
    putStrLn "0"
  else do
    let a = U.last zs
        b = U.head ws
    print $ minimum $ map abs [total - 2 * a, total - 2 * b]
