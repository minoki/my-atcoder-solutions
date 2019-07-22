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
import qualified Data.IntSet as IntSet
import Data.Foldable
import Control.Monad.ST

main = do
  [n,m] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  edges <- U.replicateM m $ do
    [u,v] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    return (u-1,v-1)
  [s,t] <- map (subtract 1) . unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let graph :: V.Vector IntSet.IntSet
      graph = V.create $ do
        vec <- VM.replicate (3 * n) IntSet.empty
        U.forM_ edges $ \(u,v) -> do
          modify' vec (IntSet.insert (3 * v + 1)) (3 * u)
          modify' vec (IntSet.insert (3 * v + 2)) (3 * u + 1)
          modify' vec (IntSet.insert (3 * v)) (3 * u + 2)
        return vec
      result = runST $ do
        visited <- UM.replicate (3 * n) False
        let !target = 3 * t
        {-
        -- bfs :: Int -> IntSet -> ST s (Maybe Int)
        let bfs !depth ss = do
              forM_IntSet ss $ \u ->
                UM.write visited u True
              let ts = foldMap_IntSet (graph V.!) ss
              ts' <- foldMapM_IntSet (\v -> do
                                         d <- UM.read visited v
                                         pure $ if d then IntSet.empty else IntSet.singleton v
                                     ) ts
              if target `IntSet.member` ts'
                then return $ Just (depth `quot` 3)
                else if IntSet.null ts'
                     then return Nothing
                     else bfs (depth+1) ts'
        bfs 1 (IntSet.singleton (3 * s))
-}
        -- bfs :: Int -> [Int] -> ST s (Maybe Int)
        let bfs !depth ss = do
              forM_ ss $ \u ->
                UM.write visited u True
              let ts = foldMap (graph V.!) ss
              if target `IntSet.member` ts
                then return $ Just (depth `quot` 3)
                else do ts' <- filterM (\v -> not <$> UM.read visited v) (IntSet.toList ts)
                        if null ts'
                          then return Nothing
                          else bfs (depth+1) ts'
        bfs 1 [3 * s]
  case result of
    Nothing -> putStrLn "-1"
    Just d -> print d

modify' :: VM.MVector s a -> (a -> a) -> Int -> ST s ()
modify' vec f !i = do x <- VM.read vec i
                      VM.write vec i $! f x

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
