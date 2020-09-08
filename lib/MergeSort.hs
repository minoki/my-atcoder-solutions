{-# LANGUAGE BangPatterns #-}
module MergeSort where
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Algorithms.Merge as A

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

sortVector :: (G.Vector v a, Ord a) => v a -> v a
sortVector v = G.create do
  v' <- G.thaw v
  A.sort v'
  return v'
{-# INLINE sortVector #-}
