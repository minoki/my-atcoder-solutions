-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.List (unfoldr)
import Control.Monad
import Control.Monad.ST
import Control.Monad.Except
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS
import qualified Data.IntSet as IntSet

-- 閉路を見つける
findCycle :: V.Vector IntSet.IntSet -> Either [Int] ()
findCycle edges = runST $ runExceptT $ do
  let !n = V.length edges  -- 頂点の個数
  seen <- UM.replicate n (0 :: Int) -- 0: new, 1: DFSの最中, 2: チェック済み
  let dfs !x path = do
        -- x を通る閉路があるかチェックする
        UM.write seen x 1
        forM_ (IntSet.toList $ edges V.! x) $ \y -> do
          -- 辺 (x,y) が存在
          s <- UM.read seen y
          case s of 0 -> dfs y (y : path)
                    1 -> throwError (y : reverse (y : takeWhile (/= y) path)) -- コールスタックのどこかで dfs y が呼ばれている。閉路
                    _ -> return ()
        UM.write seen x 2
  forM_ [0..n-1] $ \x -> do
    s <- UM.read seen x
    when (s == 0) $ dfs x [x]
  return ()

reduceCycle ::  V.Vector IntSet.IntSet -> [Int] -> [Int]
reduceCycle edges_from = reduce1
  where
    -- 閉路の一部をショートカットする経路があったらそっちに繋ぎかえる、という操作を繰り返す
    loop :: IntSet.IntSet -> IntSet.IntSet -> [Int] -> [Int] -> [Int]
    loop m seen revAcc (i:ys@(j:_)) =
      case IntSet.maxView $ IntSet.delete j $ edges_from V.! i `IntSet.intersection` m of
        Nothing -> loop m (IntSet.insert i seen) (i : revAcc) ys
        Just (j',_) | j' `IntSet.member` seen -> reduce1 (j' : reverse (j' : i : takeWhile (/= j') revAcc))
                    | otherwise -> loop m (IntSet.insert i seen) (i : revAcc) (dropWhile (/= j') ys)
    loop m seen revAcc [i] = reverse revAcc -- ショートカットできる経路はもうない
    loop _ _ _ [] = error "something is wrong"
    reduce1 :: [Int] -> [Int]
    reduce1 path = loop (IntSet.fromList path) IntSet.empty [] path

main = do
  [n,m] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  edges <- U.replicateM m $ do
    [x,y] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    return (x-1,y-1)
  let edges_from :: V.Vector IntSet.IntSet
      edges_from = V.create $ do
        vec <- VM.replicate n IntSet.empty
        U.forM_ edges $ \(a,b) ->
          VM.modify vec (IntSet.insert b) a
        return vec
  case findCycle edges_from of
    Right _ -> putStrLn "-1"
    Left path -> do
      let path' = reduceCycle edges_from path
      print $ length path'
      forM_ path' $ \i -> do
        print (i+1)
