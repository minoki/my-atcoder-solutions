-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.Int (Int64)
import Data.List (unfoldr)
import Control.Monad
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Char8 as BS
import Data.Array.Unboxed
import Data.Array.ST

warshallFloyd1 :: Int -> U.Vector (Int, Int, Int64) -> UArray (Int, Int) Int64
warshallFloyd1 !n !edges = runSTUArray $ do
  !arr <- newArray ((0,0),(n-1,n-1)) (10^9+1)
  U.forM_ edges $ \(a,b,c) -> do
    writeArray arr (a,b) c
    writeArray arr (b,a) c
  forM_ [0..n-1] $ \a -> do
    writeArray arr (a,a) 0
  forM_ [0..n-1] $ \k -> do
    forM_ [0..n-1] $ \i -> do
      forM_ [0..n-1] $ \j -> do
        a_ik <- readArray arr (i,k)
        a_kj <- readArray arr (k,j)
        a_ij <- readArray arr (i,j)
        writeArray arr (i,j) $! min a_ij (a_ik + a_kj)
  return arr

warshallFloyd2 :: Int -> Int -> UArray (Int, Int) Int64 -> UArray (Int, Int) Int
warshallFloyd2 !n !l !graph = runSTUArray $ do
  !arr <- newArray ((0,0),(n-1,n-1)) (10^9+1)
  forM_ [0..n-1] $ \a -> do
    forM_ [0..n-1] $ \b -> do
      let v = graph ! (a,b)
      when (v <= fromIntegral l) $ do
        writeArray arr (a,b) 1
  forM_ [0..n-1] $ \a -> do
    writeArray arr (a,a) 0
  forM_ [0..n-1] $ \k -> do
    forM_ [0..n-1] $ \i -> do
      forM_ [0..n-1] $ \j -> do
        a_ik <- readArray arr (i,k)
        a_kj <- readArray arr (k,j)
        a_ij <- readArray arr (i,j)
        writeArray arr (i,j) $! min a_ij (a_ik + a_kj)
  return arr

main = do
  [n,m,l] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  edges <- U.replicateM m $ do
    [a,b,c] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    return (a-1,b-1,fromIntegral c)
  let w1 = warshallFloyd1 n edges
      w2 = warshallFloyd2 n l w1
  q <- readLn
  queries <- U.replicateM q $ do
    [s,t] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    return (s,t)
  U.forM_ queries $ \(s,t) -> do
    let d = w2 ! (s-1,t-1)
    print $ if d > 10^9 then -1 else d - 1
