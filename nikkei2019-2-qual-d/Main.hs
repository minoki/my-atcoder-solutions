-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.Int (Int64)
import Data.List (unfoldr, tails)
import Control.Monad
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS
import Control.Monad.ST
import Data.Bits

query :: Int -> Int -> U.Vector Int64 -> Int64
query !i !depth vec = minimum [vec U.! (2^k - 1 + (i `shiftR` (depth - k))) | k <- [0..depth]]

queryM :: Int -> Int -> UM.MVector s Int64 -> ST s Int64
queryM !i !depth vec = minimum <$> sequence [UM.read vec (2^k - 1 + (i `shiftR` (depth - k))) | k <- [0..depth]]

fill :: Int -> Int -> Int64 -> Int -> UM.MVector s Int64 -> ST s ()
fill !i !j !x !depth vec | i < j = doFill 0 depth i j
                         | otherwise = return ()
  where
    -- Invariant: 0 <= k*2^l <= i < j <= (k+1)*2^l <= 2^depth
    doFill !k 0 !i !j | i == k, j == k+1 = UM.modify vec (min x) (2^depth - 1 + k)
                      | otherwise = error "fill"
    doFill !k l !i !j | i == (k `shiftL` l) && j == ((k+1) `shiftL` l) = UM.modify vec (min x) (2^(depth-l) - 1 + k)
                      | m <= i = doFill (2*k+1) (l-1) i j
                      | j <= m = doFill (2*k) (l-1) i j
                      | otherwise = doFill (2*k) (l-1) i m >> doFill (2*k+1) (l-1) m j
      where m = (2*k+1) `shiftL` (l-1)

main = do
  [n,m] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  specs <- fmap mergeSort $ U.replicateM m $ do
    [x,y,z] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    return (x-1,y-1,fromIntegral z)
  let depth = ceiling (logBase 2 (fromIntegral n) :: Double) :: Int
  let result :: Int64
      result = runST $ do
        vec <- UM.replicate (2^(depth+1) - 1) (10^18)
        UM.write vec (2^depth-1 + 0) 0
        U.forM_ specs $ \(l,r,c) -> do
          lx <- queryM l depth vec
          fill (l+1) (r+1) (lx+c) depth vec
        queryM (n-1) depth vec
        
            {-
  let ll = case U.unzip3 specs of (l,_,_) -> U.toList l ++ [n-1]
  let vec :: U.Vector Int64
      vec = U.create $ do
        vec <- UM.replicate n (10^18)
        UM.write vec 0 0
        forM_ (zip [0..m-1] (tail $ tails ll)) $ \(i,rest) -> do
          let (l,r,c) = specs U.! i
          lx <- UM.read vec l
          forM_ (takeWhile (<= r) rest) $ \j ->
            UM.modify vec (min (lx+c)) j
-}
            {-
        U.forM_ specs $ \(l,r,c) -> do
          lx <- UM.read vec l
          forM_ [l+1..r] $ \j ->
            UM.modify vec (min (lx+c)) j
-}
  --      return vec
  --let result = U.last vec
  print $ if result == 10^18 then -1 else result

mergeSortBy :: (U.Unbox a) => (a -> a -> Ordering) -> U.Vector a -> U.Vector a
mergeSortBy !cmp !vec = doSort vec
  where
    doSort vec | U.length vec <= 1 = vec
               | otherwise = let (xs, ys) = U.splitAt (U.length vec `quot` 2) vec
                             in merge (doSort xs) (doSort ys)
    merge xs ys = U.create $ do
      let !n = U.length xs
          !m = U.length ys
      result <- UM.new (n + m)
      let loop !i !j
            | i == n = U.copy (UM.drop (i + j) result) (U.drop j ys)
            | j == m = U.copy (UM.drop (i + j) result) (U.drop i xs)
            | otherwise = let !x = xs U.! i
                              !y = ys U.! j
                          in case cmp x y of
                               LT -> do UM.write result (i + j) x
                                        loop (i + 1) j
                               EQ -> do UM.write result (i + j) x
                                        UM.write result (i + j + 1) y
                                        loop (i + 1) (j + 1)
                               GT -> do UM.write result (i + j) y
                                        loop i (j + 1)
      loop 0 0
      return result

mergeSort :: (U.Unbox a, Ord a) => U.Vector a -> U.Vector a
mergeSort = mergeSortBy compare
