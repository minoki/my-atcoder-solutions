{-# LANGUAGE BangPatterns #-}
import Data.List
import Data.Char
import Data.Monoid
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Unboxed as U

spanWithLimit :: (U.Unbox a) => Int -> (a -> Bool) -> U.Vector a -> (U.Vector a, U.Vector a)
spanWithLimit !limit !f !vec | U.length vec <= limit = U.span f vec
                             | otherwise = U.splitAt (count 0) vec
  where count !i | i == limit || not (f (vec U.! i)) = i
                 | otherwise = count (i + 1)

main = do
  [n,m] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  -- as: a が小さい順にソートする
  as <- U.fromListN n . sort . unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  -- bc: c が大きい順にソートする
  bc <- fmap (sortBy (\(b,c) (b',c') -> compare c' c <> compare b b')) $ replicateM m $ do
    [b,c] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    return (b,c)
  let f !s !cards [] = s + U.sum cards
      f !s !cards ((!b,!c):xs) = let (d1, d2) = spanWithLimit b (< c) cards
                                 in if U.null d1 then s + U.sum cards else f (s + U.length d1 * c) d2 xs
  print $ f 0 as bc
