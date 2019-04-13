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
  s <- BS.getLine
  let bw = length $ filter (\(x,y) -> x /= y) $ zip (BS.unpack s) (cycle "01")
      wb = BS.length s - bw
  print (min bw wb)
