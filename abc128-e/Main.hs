-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Char
import Data.List
import Data.Monoid
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Unboxed as U

data Tree a = Leaf !Int !Int a
            | Bin !Int !Int !Int (Tree a) (Tree a)

getAt :: Tree a -> Int -> a
getAt (Leaf a b x) !i | a <= i && i < b = x
                      | otherwise = error "out of range"
getAt (Bin a b c l r) !i | i < b = getAt l i
                         | otherwise = getAt r i

fill :: Int -> Int -> a -> Tree a -> Tree a
fill !a !b x l@(Leaf a' b' y)
  | b < a' || b' < a = l
  -- a' <= b && a <= b':
  | a <= a' && b' <= b = Leaf a' b' x
  | a <= a' {- , b < b' -} = Bin a' b b' (Leaf a' b x) (Leaf b b' y)
  | {- a' < a, -} b' <= b = Bin a' a b' (Leaf a' a y) (Leaf a b' x)
  | otherwise {- a' < a, b < b' -} = Bin a' a b' (Leaf a' a y) (Bin a b b' (Leaf a b x) (Leaf b b' y))
fill !a !b x (Bin a' b' c' l r)
  | a <= a' && c' <= b = Leaf a b x
  | b <= b' = Bin a' b' c' (fill a b x l) r
  | b' <= a = Bin a' b' c' l (fill a b x r)
  | otherwise {- a < b', b' < b -} = Bin a' b' c' (fill a b' x l) (fill b' b x r)

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
  let result = foldl' (\r (s,t,x) ->
                         let !s' = s - x
                             !t' = t - x
                         in fill s' t' x r
                      ) (Leaf 0 (10^9+1) (-1)) works'
  U.forM_ ds $ \d -> print (getAt result d :: Int)
