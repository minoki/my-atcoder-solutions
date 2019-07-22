-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.List (unfoldr)
import Control.Monad
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS

solve :: Int -> U.Vector Int -> U.Vector Int
solve !n v = U.create $ do
  result <- UM.replicate (n + 1) 0
  forM_ [n,n-1..1] $ \i -> do
    s <- sumM [ UM.read result j | j <- [2*i,3*i..n] ]
    UM.write result i ((s + v U.! (i-1)) `rem` 2)
  return result

main = do
  n <- readLn
  xs <- U.unfoldrN n (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let result = U.filter (\(i,v) -> v > 0) $ U.indexed $ solve n xs
  print $ U.length result
  U.forM_ result $ \(i,_) -> print i

sumM :: (Monad m, Num a) => [m a] -> m a
sumM = foldM (\s a -> (s +) <$> a) 0
