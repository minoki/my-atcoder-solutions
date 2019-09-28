-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.Int (Int64)
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
import Data.Monoid
import Data.Foldable
import Debug.Trace

checkCycle :: Monad m => V.Vector IntSet.IntSet -> V.Vector IntSet.IntSet -> [Int] -> ExceptT [Int] m ()
checkCycle edges_from edges_to revPath = do
  let m = IntSet.fromList revPath
  let checkEdge i j | IntSet.size ((edges_from V.! i) `IntSet.intersection` m) > 1 = False
                    | IntSet.size ((edges_to V.! j) `IntSet.intersection` m) > 1 = False
                    | otherwise = True
  let ok1 = and (zipWith checkEdge (tail revPath) revPath)
  if ok1
    then let path = reverse revPath
         in if checkEdge (head path) (head revPath)
            then throwError path
            else return ()
    else return ()

findCycle :: V.Vector IntSet.IntSet -> V.Vector IntSet.IntSet -> Either [Int] ()
findCycle edges edges_to = runST $ runExceptT $ do
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

reduceCycle ::  V.Vector IntSet.IntSet -> V.Vector IntSet.IntSet -> [Int] -> [Int]
reduceCycle edges_from edges_to path = do
  let loop :: IntSet.IntSet -> [Int] -> IntSet.IntSet -> [Int] -> [Int]
      loop m (i:ys@(j:_)) seen revAcc = do
        let s1 = IntSet.delete j ((edges_from V.! i) `IntSet.intersection` m)
        case IntSet.maxView s1 of
          Nothing -> loop m ys (IntSet.insert i seen) (i : revAcc)
          Just (j',_) | j' `IntSet.member` seen -> loopX (j' : reverse (j' : i : takeWhile (/= j') revAcc))
                      | otherwise -> case dropWhile (/= j') ys of
                                       [] -> error $ show (i,j',ys,seen,revAcc)
                                       ys' -> loop m ys' (IntSet.insert i seen) (i : revAcc)
      loop m [i] seen revAcc = reverse revAcc
      loopX path = loop (IntSet.fromList path) path IntSet.empty []
  loopX path

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
  let edges_to :: V.Vector IntSet.IntSet
      edges_to = V.create $ do
        vec <- VM.replicate n IntSet.empty
        U.forM_ edges $ \(a,b) ->
          VM.modify vec (IntSet.insert a) b
        return vec
  case findCycle edges_from edges_to of
    Right _ -> putStrLn "-1"
    Left path -> do
      let path' = reduceCycle edges_from edges_to path
      print $ length path'
      forM_ path' $ \i -> do
        print (i+1)

foldMap_IntSet :: (Monoid n) => (Int -> n) -> IntSet.IntSet -> n
foldMap_IntSet f set = go set
  where
    go set = case IntSet.splitRoot set of
               [] -> mempty
               [x] -> foldMap f (IntSet.toList x)
               xs -> foldMap go xs

foldMapM_IntSet :: (Monoid n, Monad m) => (Int -> m n) -> IntSet.IntSet -> m n
foldMapM_IntSet f set = go set
  where
    go set = case IntSet.splitRoot set of
               [] -> return mempty
               [x] -> foldlM (\x v -> mappend x <$> f v) mempty (IntSet.toList x)
               xs -> foldlM (\x set' -> mappend x <$> go set') mempty xs
