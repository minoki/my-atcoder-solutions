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
  [n,k] :: [Int] <- map (read . BS.unpack) . BS.words <$> BS.getLine
  s <- BS.getLine -- '0' or '1'
  -- standing: 0, headstanding: 1
  let countStanding s | BS.null s = []
                      | (a,b) <- BS.span (== '0') s = BS.length a : countHandstanding b
      countHandstanding s | BS.null s = [0]
                          | (a,b) <- BS.span (== '1') s = BS.length a : countStanding b
      standing :: U.Vector Int
      standing = U.fromListN n $ countHandstanding s
      ss = U.scanl' (+) 0 standing
  if U.length ss > 2 * k + 1
    then print $ maximum [ss U.! (2*(i+k)+1) - ss U.! (2 * i) | i <- [0..(U.length ss `quot` 2) - k - 1]]
    else print n
