{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Control.Monad
import Data.Int (Int64)
import Data.List (foldl')
import Data.Array
import Data.Array.Unboxed
import Data.Array.ST
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS

main = do
  _ :: Int <- readLn
  heights :: [Int] <- map (read . BS.unpack) . BS.words <$> BS.getLine
  let (_,n) = foldl' (\(maxH,n) h -> if maxH <= h then let !n'=n+1 in (h,n') else (maxH,n)) (0,0) heights
  print (n :: Int)
