-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.Int (Int64)
import Data.Bifunctor (first)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Unboxed as U

-- skipping 3 (U.fromList [0,1,2,3,4,5]) = [0,3]
skipping :: (U.Unbox a) => Int -> U.Vector a -> U.Vector a
skipping !d !v = U.generate ((U.length v + d - 1) `quot` d) $ \i -> v U.! (i * d)

main = do
  n <- readLn
  ss <- U.unfoldrN n (readInt64 . BS.dropWhile isSpace) <$> BS.getLine
  let ts = U.zipWith (+) ss (U.reverse ss)
  print $ maximum $ [ U.maximum $ U.scanl (+) 0 (skipping d $ U.take l ts)
                    | d <- [1..n-2]
                    , let l | (n - 1) `rem` d == 0 = min (n `quot` 2) (n - 1 - d)
                            | otherwise = n - 1 - d
                    ]

readInt64 :: BS.ByteString -> Maybe (Int64, BS.ByteString)
readInt64 s = first fromIntegral <$> BS.readInt s
