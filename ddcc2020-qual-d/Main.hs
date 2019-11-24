-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.List (unfoldr)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Char8 as BS
import Data.Bits

nfIntPair :: (Int,Int) -> (Int,Int)
nfIntPair x@(!_,!_) = x

f :: (Int,Int) -> (Int,Int) -> (Int,Int)
f (x,m) (y,n) = case (x + y) `quotRem` 10 of
          (a,b) -> nfIntPair (a + b, m + n + 1 + a)

gen :: Int -> U.Vector (Int,Int)
gen a = U.iterateN 64 (\x -> f x x) (a,0)

computed :: V.Vector (U.Vector (Int,Int))
computed = V.fromList $ map gen [0..9]

pow' :: Int -> Int -> (Int,Int)
pow' a n = foldr1 f [ computed V.! a U.! i | i <- [0..60], testBit n i ]

main = do
  m <- readLn
  xs <- U.replicateM m $ do
    [d,c] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    -- c should be Int64
    return (pow' d c)
  print $ snd $ U.foldr1 f xs
