-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.Int
import Data.List (unfoldr)
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Char8 as BS

-- let k = search v f i j
-- => (k < j || f k) && (k == i || not (f (k-1)))
search :: (U.Unbox a) => U.Vector a -> (a -> Bool) -> Int -> Int -> Int
search !v !f !i !j
  | i >= j = error "bad input"
  | f (v U.! i) = i
  | otherwise = loop i j
  where loop !i !j | j == i + 1 = j
                   | f (v U.! k) = loop i k
                   | otherwise = loop k j
          where k = (i + j) `quot` 2


main = do
  [n,k] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  xs <- U.map fromIntegral . U.unfoldrN n (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let ys :: U.Vector Int64
      ys = U.scanl' (+) 0 xs
      loop !acc !i | i >= U.length ys = acc
                   | ys U.! i < fromIntegral k = loop acc (i+1)
                   | otherwise = let y = ys U.! i
                                     j = search ys (> y - fromIntegral k) 0 i
                                 in loop (acc + fromIntegral j) (i+1)
  print (loop 0 1 :: Int64)
