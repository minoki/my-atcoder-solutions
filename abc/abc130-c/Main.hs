-- https://github.com/minoki/my-atcoder-solutions
import Data.Char (isSpace)
import Data.List (unfoldr)
import qualified Data.ByteString.Char8 as BS

main = do
  [w,h,x,y] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let s = fromIntegral w * fromIntegral h / 2
  let t | x * 2 == w && y * 2 == h = 1
        | otherwise = 0
  putStrLn $ unwords [show s, show t]
