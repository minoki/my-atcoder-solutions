-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Char
import Data.List
import Data.Monoid
import Data.Bits
import Control.Monad
import Control.Monad.ST
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

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

query :: Int -> Int -> U.Vector Int -> Int
query !i !depth vec = minimum [vec U.! (2^k - 1 + (i `shiftR` (depth - k))) | k <- [0..depth]]

fill :: Int -> Int -> Int -> Int -> UM.MVector s Int -> ST s ()
fill !i !j !x !depth vec | i < j = doFill 0 depth i j
                         | otherwise = return ()
  where
    -- Invariant: 0 <= k*2^l <= i < j <= (k+1)*2^l <= 2^depth
    doFill !k 0 !i !j | i == k, j == k+1 = UM.write vec (2^depth - 1 + k) x
                      | otherwise = error "fill"
    doFill !k l !i !j | i == (k `shiftL` l) && j == ((k+1) `shiftL` l) = UM.write vec (2^(depth-l) - 1 + k) x
                      | m <= i = doFill (2*k+1) (l-1) i j
                      | j <= m = doFill (2*k) (l-1) i j
                      | otherwise = doFill (2*k) (l-1) i m >> doFill (2*k+1) (l-1) m j
      where m = (2*k+1) `shiftL` (l-1)

main = do
  [n,q] <- map (read . BS.unpack) . BS.words <$> BS.getLine
  -- 1 <= n <= 2*10^5, 1 <= q <= 2*10^5
  works <- replicateM n $ do
    [s,t,x] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    return (s,t,x)
  let works' = sortBy (\(s,t,x) (s',t',x') -> compare x' x <> compare s s' <> compare t t') works
  ds <- U.replicateM q $ do
    Just (d, _) <- BS.readInt <$> BS.getLine
    return d
  -- q <= 2^depth
  let depth = ceiling (logBase 2 (fromIntegral q) :: Double) :: Int
  let result = U.create $ do
        vec <- UM.replicate (2^(depth+1)-1) (10^9+1)
        forM_ works' $ \(s,t,x) -> do
          let !s' = s - x
              !t' = t - x
              i0 = search ds (\d -> s' <= d) 0 q
              i1 | i0 == q = q
                 | otherwise = search ds (\d -> t' <= d) i0 q
          fill i0 i1 x depth vec
        return vec
  forM_ [0..q-1] $ \i -> do
    let v = query i depth result
    if v == 10^9+1
      then putStrLn "-1"
      else print v
