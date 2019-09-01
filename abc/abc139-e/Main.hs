-- https://github.com/minoki/my-atcoder-solutions
-- {-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.List (unfoldr)
import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS

-- 有向グラフに閉路があるか判定する
hasCycle :: V.Vector [Int] -> Bool
hasCycle edges = (== Nothing) $ runST $ runMaybeT $ do
  let !n = V.length edges  -- 頂点の個数
  seen <- UM.replicate n (0 :: Int) -- 0: new, 1: DFSの最中, 2: チェック済み
  let dfs !x = do
        -- x を通る閉路があるかチェックする
        UM.write seen x 1
        forM_ (edges V.! x) $ \y -> do
          -- 辺 (x,y) が存在
          s <- UM.read seen y
          case s of 0 -> dfs y
                    1 -> mzero -- コールスタックのどこかで dfs y が呼ばれている。閉路
                    _ -> return ()
        UM.write seen x 2
  forM_ [0..n-1] $ \x -> do
    s <- UM.read seen x
    when (s == 0) $ dfs x
  return ()

-- DAGの最長路の長さを返す
longest :: V.Vector [Int] -> Int
longest edges = U.maximum $ U.create $ do
  let n = V.length edges
  vec <- UM.replicate n (-1 :: Int)
  let dfs !x = do
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
  let m = n * (n - 1) `quot` 2  -- グラフの頂点数
  let -- (i,j) (0 <= i < j < n) を 0 以上 m 未満の整数値にコードする
      makePair :: Int -> Int -> Int
      makePair i j | i < j = n * i - (i + 1) * i `quot` 2 + j - i - 1
                   | otherwise = undefined
  -- 「試合 (i,j) よりも試合 (i',j') の方を後に行う場合に辺 makePair i j → makePair i' j' が存在する」ようなグラフを作る
  let edges :: V.Vector [Int]
      edges = V.create $ do
        e <- VM.replicate m []
        V.forM_ (V.indexed xs) $ \(i,ys) -> do
          U.forM_ (U.zip ys (U.tail ys)) $ \(a,b) -> do
            -- (i,a) -> (i,b)
            let !b' = makePair (min i b) (max i b)
            VM.modify e (b' :) (makePair (min i a) (max i a))
        return e
  print $ if hasCycle edges
          then -1
          else longest edges
