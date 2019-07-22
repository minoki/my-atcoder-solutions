-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.Int (Int64)
import Data.Bifunctor (first)
import Data.List
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Unboxed as U

-- skipping 3 (U.fromList [0,1,2,3,4,5]) = [0,3]
skipping :: (U.Unbox a) => Int -> U.Vector a -> [a]
skipping !d !v = [ v U.! i | i <- [0,d..U.length v - 1] ]

main = do
  n <- readLn
  ss <- U.unfoldrN n (readInt64 . BS.dropWhile isSpace) <$> BS.getLine
  let ts = U.zipWith (+) ss (U.reverse ss)
  print $ maximum $ [ maximum $ scanl' (+) 0 (skipping d $ U.take l ts)
                    | d <- [1..n-2]
                    , let l = if (n - 1) `rem` d == 0
                              then min (n `quot` 2) (n - 1 - d)
                              else n - 1 - d
                    ]

readInt64 :: BS.ByteString -> Maybe (Int64, BS.ByteString)
readInt64 s = first fromIntegral <$> BS.readInt s
