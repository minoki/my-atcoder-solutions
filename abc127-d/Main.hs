-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.List
import Data.Char
import Data.Monoid
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

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

spanWithLimit :: (U.Unbox a) => Int -> (a -> Bool) -> U.Vector a -> (U.Vector a, U.Vector a)
spanWithLimit !limit !f !vec | U.length vec <= limit = U.span f vec
                             | otherwise = U.splitAt (count 0) vec
  where count !i | i == limit || not (f (vec U.! i)) = i
                 | otherwise = count (i + 1)

main = do
  [n,m] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  -- as: a が小さい順にソートする
  as <- mergeSortBy compare . U.unfoldrN n (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  -- bc: c が大きい順にソートする
  bc <- fmap (U.toList . mergeSortBy (\(b,c) (b',c') -> compare c' c <> compare b b')) $ U.replicateM m $ do
    [b,c] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    return (b,c)
  let f !s !cards [] = s + U.sum cards
      f !s !cards ((!b,!c):xs) = let (d1, d2) = spanWithLimit b (< c) cards
                                 in if U.null d1 then s + U.sum cards else f (s + U.length d1 * c) d2 xs
  print $ f 0 as bc
