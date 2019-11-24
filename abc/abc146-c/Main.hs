-- https://github.com/minoki/my-atcoder-solutions
import Data.Char (isSpace)
import Data.List (unfoldr)
import qualified Data.ByteString.Char8 as BS

main = do
  [a,b,x] <- unfoldr (BS.readInteger . BS.dropWhile isSpace) <$> BS.getLine
  -- a*n + b*d <= x
  -- a*n <= x - b*d
  print $ maximum $
    [0]
    ++ [ 10^9 | a*10^9+b*10 <= x ]
    ++ [ min n (10^d-1) | d <- [1..9]
                        , x - b*d >= 0
                        , let n = (x - b*d) `quot` a
                        , 10^(d-1) <= n
                        ]
