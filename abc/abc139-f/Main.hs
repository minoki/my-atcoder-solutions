-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.List (unfoldr, sort)
import Control.Monad
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Char8 as BS
import Data.Monoid

sub :: (Int, Int) -> (Int, Int) -> (Int, Int)
sub (x,y) (x',y') = (x-x',y-y')

main = do
  n <- readLn
  -- n <= 100 なのでリストの sort を使うのでも十分速い
  -- (x,y) == (0,0) の場合は atan2 はよくわからない値になるかもしれないが、エラーにさえならなければソート結果のどこに入っていても問題ない
  xs <- fmap (U.fromListN n . sort) $ replicateM n $ do
    [x,y] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    return (atan2 (fromIntegral y) (fromIntegral x) :: Double, x, y)
  let neg = U.takeWhile (\(a,_,_) -> a <= 0) xs -- x軸の下にある部分
  let xs' = xs <> U.map (\(a,x,y) -> (a + 2 * pi, x, y)) neg -- 1週半させる
  let ys = U.scanl' (\(!sx,!sy) (_,x,y) -> (sx+x,sy+y)) (0,0) xs'
  let zs = [ (ys U.! (j+1)) `sub` (ys U.! i) | i <- [0..n-1], j <- [i..min (i+n-1) (U.length xs' - 1)]]
  print $ maximum $ map (\(x,y) -> sqrt ((fromIntegral x)^2 + (fromIntegral y)^2)) zs
