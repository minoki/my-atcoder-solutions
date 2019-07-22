-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
module MainNaive where
import Data.Char
import Data.List
import Data.Monoid
import Control.Monad
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

main = do
  [n,q] <- map (read . BS.unpack) . BS.words <$> BS.getLine
  -- 1 <= n <= 2*10^5, 1 <= q <= 2*10^5
  works <- replicateM n $ do
    [s,t,x] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    return (s,t,x)
  let works' = U.fromListN n $ sortBy (\(s,t,x) (s',t',x') -> compare x' x <> compare s s' <> compare t t') works
  ds <- U.replicateM q $ do
    Just (d, _) <- BS.readInt <$> BS.getLine
    return d
  let result = U.create $ do
        vec <- UM.replicate q (-1)
        U.forM_ works' $ \(s,t,x) -> do
          let !s' = s - x
              !t' = t - x
              i0 = search ds (\d -> s' <= d) 0 q
              -- loop !i | i == q || t' <= ds U.! i = return ()
              --         | otherwise = UM.write vec i x >> loop (i+1)
              i1 | i0 == q = q
                 | otherwise = search ds (\d -> t' <= d) i0 q
          UM.set (UM.slice i0 (i1 - i0) vec) x
          -- forM_ [i0..(min i1 q)-1] $ \i -> do
          --   UM.unsafeWrite vec i x
          -- loop i0
        return vec
  U.forM_ result print
