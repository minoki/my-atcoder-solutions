-- https://github.com/minoki/my-atcoder-solutions
import Data.Char (isSpace)
import Data.List
import Control.Monad
import qualified Data.ByteString.Char8 as BS

main = do
  [a1,a2,a3] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  -- a1 >= a2 >= a3
  let n = a1 + a2 + a3
  print $ length $ do
    -- 1 <= x <= n
    xs <- permutations [1..n]
    let (x1,xs') = splitAt a1 xs
        (x2,x3) = splitAt a2 xs'
    guard $ and $ zipWith (>) (tail x1) x1
    guard $ and $ zipWith (>) (tail x2) x2
    guard $ and $ zipWith (>) (tail x3) x3
    guard $ and $ zipWith (>) x2 x1
    guard $ and $ zipWith (>) x3 x2
    return ()
