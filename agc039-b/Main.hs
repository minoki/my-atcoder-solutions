-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Maybe
import Data.Monoid
import Data.Foldable
import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS
import qualified Data.IntSet as IntSet

farthest :: V.Vector IntSet.IntSet -> Int -> Int
farthest graph !origin = U.maximum d
  where
    d = U.create $ do
      let n = V.length graph
      vec <- UM.replicate n (-1 :: Int)
      let bfs !depth !seen !set
            | IntSet.null set = return ()
            | otherwise = do
                forM_ (IntSet.toList set) $ \i -> do
                  UM.write vec i depth
                let seen' = set `IntSet.union` seen
                let next = foldMap_IntSet (graph V.!) set `IntSet.difference` seen'
                bfs (depth + 1) seen' next
      bfs 0 IntSet.empty (IntSet.singleton origin)
      return vec

solve :: V.Vector IntSet.IntSet -> Bool
solve graph = fromMaybe False $ runST $ runMaybeT $ do
  let n = V.length graph
  result <- UM.replicate n (0 :: Int)
  let dfs !p !i !c = do
        c' <- UM.read result i
        case c' of
          0 -> do
            UM.write result i c
            forM_ (IntSet.toList $ graph V.! i) $ \j -> do
              when (j /= p) $ do
                dfs i j (-c)
          _ | c' == c -> return ()
            | otherwise -> mzero
  dfs (-1) 0 1
  return True

main = do
  n <- readLn
  s <- V.replicateM n BS.getLine
  -- s V.! i `BS.index` j
  let graph :: V.Vector IntSet.IntSet
      graph = V.create $ do
        g <- VM.replicate n IntSet.empty
        V.forM_ (V.indexed s) $ \(i,r) -> do
          forM_ [0..n-1] $ \j -> do
            let c = r `BS.index` j
            when (c == '1') $ do
              VM.modify g (IntSet.insert j) i
        return g
  if solve graph then do
    let y = maximum $ map (farthest graph) [0..n-1]
    print (y + 1)
  else
    putStrLn "-1"

foldMap_IntSet :: (Monoid n) => (Int -> n) -> IntSet.IntSet -> n
foldMap_IntSet f set = go set
  where
    go set = case IntSet.splitRoot set of
               [] -> mempty
               [x] -> foldMap f (IntSet.toList x)
               xs -> foldMap go xs
