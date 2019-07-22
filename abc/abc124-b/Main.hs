{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Data.List (foldl')
import qualified Data.ByteString.Char8 as BS

main = do
  _ :: Int <- readLn
  heights :: [Int] <- map (read . BS.unpack) . BS.words <$> BS.getLine
  let (_,n) = foldl' (\(maxH,n) h -> if maxH <= h then let !n'=n+1 in (h,n') else (maxH,n)) (0,0) heights
  print (n :: Int)
