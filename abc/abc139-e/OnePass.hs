-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.List (unfoldr)
import Data.Maybe (fromMaybe)
import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class (lift)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS

-- 有向グラフに閉路があるか判定し、閉路があれば Nothing を返す。
-- 閉路がなければ最長路の長さを返す。
solve :: V.Vector [Int] -> Maybe Int
solve !edges = runST $ runMaybeT $ do
  let !n = V.length edges  -- 頂点の個数
  !vec <- UM.replicate n (-2 :: Int) -- -2: new, -1: DFSの最中, 正: チェック済み
  let dfs !x = do
        s <- UM.read vec x
        case s of
          -2 -> do
            -- x を通る閉路があるかチェックする
            UM.write vec x (-1)
            !v <- foldM (\ !a y -> max a <$> dfs y) 0 (edges V.! x)
            let !v' = v + 1
            UM.write vec x v'
            return v'
          -1 -> mzero  -- コールスタックのどこかで dfs x が呼ばれている。閉路
          v -> return v
  mapM_ dfs [0..n-1]
  lift (U.maximum <$> U.unsafeFreeze vec)

main = do
  n <- readLn
  xs <- V.replicateM n $ do
    U.map (subtract 1) . U.unfoldrN (n-1) (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let m = n * (n - 1) `quot` 2  -- グラフの頂点数
  let -- (i,j) (0 <= i < j < n) を 0 以上 m 未満の整数値にコードする
      makePair :: Int -> Int -> Int
      makePair !i !j | i' < j' = n * i' - (i' + 1) * i' `quot` 2 + j' - i' - 1
                     | otherwise = undefined
        where !i' = min i j; !j' = max i j
  -- 「試合 (i,j) よりも試合 (i',j') の方を後に行う場合に辺 makePair i j → makePair i' j' が存在する」ようなグラフを作る
  let edges :: V.Vector [Int]
      edges = V.create $ do
        !e <- VM.replicate m []
        V.forM_ (V.indexed xs) $ \(!i,ys) -> do
          U.forM_ (U.zip ys (U.tail ys)) $ \(!a,!b) -> do
            -- (i,a) -> (i,b)
            let !b' = makePair i b
            VM.modify e (b' :) (makePair i a)
        return e
  print $ fromMaybe (-1) $ solve edges
