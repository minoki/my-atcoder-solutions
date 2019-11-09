-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.List (unfoldr)
import Control.Monad
import Control.Monad.ST
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Builder as BSB
import System.IO (stdout)
import Data.Monoid

warshallFloyd1 :: Int -> U.Vector (Int, Int, Int) -> V.Vector (U.Vector Int)
warshallFloyd1 !n !edges = runST $ do
  !arr <- V.replicateM n (UM.replicate n (10^9+1))
  U.forM_ edges $ \(a,b,c) -> do
    UM.unsafeWrite (arr `V.unsafeIndex` a) b c
    UM.unsafeWrite (arr `V.unsafeIndex` b) a c
  forM_ [0..n-1] $ \a -> do
    UM.unsafeWrite (arr `V.unsafeIndex` a) a 0
  forM_ [0..n-1] $ \k -> do
    let !a_k = arr `V.unsafeIndex` k
    forM_ [0..n-1] $ \i -> do
      let !a_i = arr `V.unsafeIndex` i
      a_ik <- UM.unsafeRead a_i k
      forM_ [0..n-1] $ \j -> do
        a_kj <- UM.unsafeRead a_k j
        UM.unsafeModify a_i (min (a_ik + a_kj)) j
  V.mapM U.unsafeFreeze arr

warshallFloyd2 :: Int -> Int -> V.Vector (U.Vector Int) -> V.Vector (U.Vector Int)
warshallFloyd2 !n !l !graph = runST $ do
  !arr <- V.replicateM n (UM.replicate n (10^9+1))
  forM_ [0..n-1] $ \a -> do
    let !a_a = arr `V.unsafeIndex` a
    let !g_a = graph `V.unsafeIndex` a
    forM_ [0..n-1] $ \b -> do
      let v = g_a `U.unsafeIndex` b
      when (v <= fromIntegral l) $ do
        UM.unsafeWrite a_a b 1
    UM.unsafeWrite a_a a 0
  forM_ [0..n-1] $ \k -> do
    let !a_k = arr `V.unsafeIndex` k
    forM_ [0..n-1] $ \i -> do
      let !a_i = arr `V.unsafeIndex` i
      a_ik <- UM.unsafeRead a_i k
      forM_ [0..n-1] $ \j -> do
        a_kj <- UM.unsafeRead a_k j
        UM.unsafeModify a_i (min (a_ik + a_kj)) j
  V.mapM U.unsafeFreeze arr

main = do
  [n,m,l] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  edges <- U.replicateM m $ do
    [a,b,c] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    return (a-1,b-1,c)
  let !w1 = warshallFloyd1 n edges
      !w2 = warshallFloyd2 n l w1
  q <- readLn
  queries <- U.replicateM q $ do
    [s,t] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    return (s,t)
  BSB.hPutBuilder stdout $ mconcat [ BSB.intDec (if d > 10^9 then -1 else d - 1) <> BSB.char7 '\n' | (s,t) <- U.toList queries, let d = w2 `V.unsafeIndex` (s-1) `U.unsafeIndex` (t-1) ]
