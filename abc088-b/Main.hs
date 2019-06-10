-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.List (unfoldr, sort)
import qualified Data.ByteString.Char8 as BS
import Data.Coerce (coerce)
import Data.Ord (Down(..))

sortDown :: forall a. Ord a => [a] -> [a]
sortDown = coerce (sort :: [Down a] -> [Down a])

main = do
  n :: Int <- readLn
  xs <- sortDown . unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let solve !d [] = d
      solve !d [x] = x + d
      solve !d (x:y:xs) = solve (d + x - y) xs
  print $ solve 0 xs
