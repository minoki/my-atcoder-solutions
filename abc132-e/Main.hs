-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.List (unfoldr)
import Control.Monad
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Foldable
import Control.Monad.ST
import Debug.Trace

main = do
  [n,m] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  edges <- U.replicateM m $ do
    [u,v] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    return (u-1,v-1)
  [s,t] <- map (subtract 1) . unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let {-
      graph1 :: V.Vector IntSet.IntSet
      graph1 = V.create $ do
        vec <- VM.replicate n IntSet.empty
        U.forM_ edges $ \(u,v) -> do
          s <- VM.read vec u
          VM.write vec u $! IntSet.insert v s
        return vec
      -}
      graph1 :: IntMap.IntMap IntSet.IntSet
      graph1 = U.foldl' (\m (u,v) -> IntMap.insertWith IntSet.union u (IntSet.singleton v) m) IntMap.empty edges
      graph2 :: IntMap.IntMap IntSet.IntSet
      graph2 = IntMap.map (foldMap_IntSet (go 2)) graph1
        where go 0 !u = IntSet.singleton u
              go 1 !u = IntMap.findWithDefault IntSet.empty u graph1
              go i !u = foldMap_IntSet (go (i - 1)) $ IntMap.findWithDefault IntSet.empty u graph1
      {-
      graph2 :: V.Vector IntSet.IntSet
      graph2 = V.create $ do
        vec <- VM.new n
        let go 0 !u = IntSet.singleton u
            go 1 !u = graph1 V.! u
            go i !u = foldMap_IntSet (go (i - 1)) $ graph1 V.! u
        forM_ [0..n-1] $ \u -> do
          VM.write vec u $! go 3 u
        return vec
      -}
      result = runST $ do
        visited <- UM.replicate n False
        {-
        -- bfs :: IntSet -> ST s (Maybe Int)
        let bfs !depth ss = do
              forM_IntSet ss $ \u ->
                UM.write visited u True
              let ts = foldMap_IntSet (graph2 V.!) ss
              ts' <- foldMapM_IntSet (\v -> do
                                         d <- UM.read visited v
                                         pure $ if d then IntSet.empty else IntSet.singleton v
                                     ) ts
              if t `IntSet.member` ts'
                then return (Just depth)
                else if IntSet.null ts'
                     then return Nothing
                     else bfs (depth+1) ts'
        bfs 1 (IntSet.singleton s)
        -}
        -- bfs :: [Int] -> ST s (Maybe Int)
        let bfs !depth ss = do
              forM_ ss $ \u ->
                UM.write visited u True
              --let ts = foldMap (graph2 V.!) ss
              let ts = foldMap (\u -> IntMap.findWithDefault IntSet.empty u graph2) ss
              if t `IntSet.member` ts
                then return (Just depth)
                else do ts' <- filterM (\v -> not <$> UM.read visited v) (IntSet.toList ts)
                        if null ts'
                          then return Nothing
                          else bfs (depth+1) ts'
        bfs 1 [s]
  -- print graph1
  -- print graph2
  case result of
    Nothing -> putStrLn "-1"
    Just d -> print d

foldMap_IntSet :: (Monoid n) => (Int -> n) -> IntSet.IntSet -> n
foldMap_IntSet f set = go set
  where
    go set = case IntSet.splitRoot set of
               [] -> mempty
               [x] -> foldMap f (IntSet.toList x)
               xs -> foldMap go xs

forM_IntSet :: Monad m => IntSet.IntSet -> (Int -> m ()) -> m ()
forM_IntSet set f = go set
  where
    go set = case IntSet.splitRoot set of
               [] -> return ()
               [x] -> forM_ (IntSet.toList x) f
               xs -> forM_ xs go

foldMapM_IntSet :: (Monoid n, Monad m) => (Int -> m n) -> IntSet.IntSet -> m n
foldMapM_IntSet f set = go set
  where
    go set = case IntSet.splitRoot set of
               [] -> return mempty
               [x] -> foldlM (\x v -> mappend x <$> f v) mempty (IntSet.toList x)
               xs -> foldlM (\x set' -> mappend x <$> go set') mempty xs
