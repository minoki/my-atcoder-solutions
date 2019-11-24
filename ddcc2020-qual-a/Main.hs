-- https://github.com/minoki/my-atcoder-solutions
import Data.List (unfoldr)
import Data.Char (isSpace)
import qualified Data.ByteString.Char8 as BS

main = do
  [x,y] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let a :: Int
      a = case x of
            3 -> 100000
            2 -> 200000
            1 -> 300000
            _ -> 0
  let b = case y of
            3 -> 100000
            2 -> 200000
            1 -> 300000
            _ -> 0
  let c = case (x,y) of
            (1,1) -> 400000
            _ -> 0
  print $ a+b+c
