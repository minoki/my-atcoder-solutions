-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.Int (Int64)
import Data.List (unfoldr)
import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS

topologicalSort :: V.Vector [Int] -> Maybe (U.Vector Int)
topologicalSort edges_from = runST $ runMaybeT $ do
  let !n = V.length edges_from -- 頂点の個数
  vec <- UM.new n
  seen <- UM.replicate n False
  iref <- UM.replicate 1 (n - 1)
  seen <- UM.replicate n (0 :: Int) -- 0: new, 1: active, 2: finished
  let dfs x = do
        UM.write seen x 1
        forM_ (edges_from V.! x) $ \y -> do
          -- 辺 (x,y) が存在
          s <- UM.read seen y
          case s of
            0 -> dfs y
            1 -> mzero
            _ -> return ()
        UM.write seen x 2
        i <- UM.read iref 0
        UM.write vec i x
        UM.write iref 0 (i - 1)
  forM_ [0..n-1] $ \x -> do
    s <- UM.read seen x
    when (s == 0) $ dfs x
  U.unsafeFreeze vec

longest :: Int -> V.Vector [Int] -> Int
longest !n edges = U.maximum $ U.create $ do
  vec <- UM.replicate n (-1 :: Int)
  let dfs x = do
        v <- UM.read vec x
        if v == -1
          then do ys <- mapM dfs (edges V.! x)
                  let !v = 1 + maximum (0 : ys)
                  UM.write vec x v
                  return v
          else return v
  forM_ [0..n-1] $ \i -> dfs i
  return vec

main = do
  n <- readLn
  xs <- V.replicateM n $ do
    U.map (subtract 1) . U.unfoldrN (n-1) (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let makePair i j | i < j = n * i - (i + 1) * i `quot` 2 + j - i - 1
                   | otherwise = undefined
  let edges :: V.Vector [Int]
      edges = V.create $ do
        e <- VM.replicate (n * (n - 1) `quot` 2) []
        V.forM_ (V.indexed xs) $ \(i,ys) -> do
          U.forM_ (U.zip ys (U.tail ys)) $ \(a,b) -> do
            -- (i,a) -> (i,b)
            let !b' = makePair (min i b) (max i b)
            VM.modify e (b' :) (makePair (min i a) (max i a))
        return e

  let p :: U.Vector (Int, Int)
      p = U.create $ do
        vec <- UM.new (n * (n - 1) `quot` 2)
        forM_ [0..n-1] $ \i -> do
          forM_ [i+1..n-1] $ \j -> do
            UM.write vec (makePair i j) (i,j)
        return vec

  let sorted = topologicalSort edges
  case sorted of
    Nothing -> putStrLn "-1"
    Just sorted -> do
      print $ longest (n * (n - 1) `quot` 2) edges
