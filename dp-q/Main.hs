-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.Int (Int64)
import Data.List (unfoldr)
import Data.Bifunctor (first)
import Control.Monad
import Control.Monad.ST
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS
import Data.Bits

type SegTree a = (Int, U.Vector a)
type MSegTree s a = (Int, U.MVector s a)

queryM :: Int -> MSegTree s Int64 -> ST s Int64
queryM !i (!depth, vec) = UM.read vec (2^depth - 1 + i)

-- queryRangeM i j st == maximum <$> sequence [query k st | k <- [i..j-1]]
queryRangeM :: Int -> Int -> MSegTree s Int64 -> ST s Int64
queryRangeM !i !j (!depth, vec) | i < j = doQuery 0 depth i j
                                | otherwise = return minBound
  where
    -- Invariant: 0 <= k*2^l <= i < j <= (k+1)*2^l <= 2^depth
    doQuery !k 0 !i !j | i == k, j == k+1 = UM.read vec (2^depth - 1 + k)
                       | otherwise = error "query"
    doQuery !k l !i !j | i == (k `shiftL` l), j == (k+1) `shiftL` l = UM.read vec (2^(depth-l) - 1 + k)
                       | m <= i = doQuery (2*k+1) (l-1) i j
                       | j <= m = doQuery (2*k) (l-1) i j
                       | otherwise = max <$> doQuery (2*k) (l-1) i m <*> doQuery (2*k+1) (l-1) m j
      where m = (2*k+1) `shiftL` (l-1)

set :: Int -> MSegTree s Int64 -> Int64 -> ST s ()
set !i (!depth, vec) !x = forM_ [0..depth] $ \k ->
  UM.modify vec (max x) (2^k - 1 + (i `shiftR` (depth - k)))

main = do
  n <- readLn
  hs <- U.unfoldrN n (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  as <- U.unfoldrN n (readInt64 . BS.dropWhile isSpace) <$> BS.getLine
  let ht = U.create $ do
        ht <- UM.new n
        flip U.imapM_ hs $ \i h -> do
          UM.write ht (h - 1) i
        return ht
  let (h2i,m) = runST $ do
        h2i <- UM.new n
        let loop !i !j !k | i == n = return k
                          | otherwise = do
                              let j' = ht U.! i
                                  k' = if j < j' then k else k+1
                              UM.write h2i i k'
                              loop (i+1) j' k'
        k <- loop 0 0 0
        h2i <- U.unsafeFreeze h2i
        return (h2i,k+1)
  let depth = ceiling (logBase 2 (fromIntegral m) :: Double) :: Int
  let result = U.create $ do
        vec <- UM.replicate (2^(depth+1)-1) 0
        let st = (depth, vec)
        forM_ [0..n-1] $ \i -> do
          let h = hs U.! i
              k = h2i U.! (h-1)
          x <- queryRangeM 0 (k+1) st
          set k st (x + as U.! i)
          {-
          x <- foldM (\x a -> max x <$> a) 0 [ UM.read vec j | j <- [0..k] ]
          UM.write vec k (x + as U.! i)
          -}
        return vec
  print $ U.head result

readInt64 :: BS.ByteString -> Maybe (Int64, BS.ByteString)
readInt64 s = first fromIntegral <$> BS.readInt s
