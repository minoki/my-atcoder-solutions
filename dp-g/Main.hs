{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Control.Monad
import Data.Int
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS
import qualified Data.IntSet as IntSet
-- import Data.Bifunctor

main = do
  [n,m] :: [Int] <- map (read . BS.unpack) . BS.words <$> BS.getLine
  -- 2 <= n <= 10^5, 1 <= m <= 10^5
  edges :: U.Vector (Int, Int) <- U.replicateM m $ do
    [x,y] <- map (read . BS.unpack) . BS.words <$> BS.getLine
    -- 1 <= x, y <= n
    return (x - 1, y - 1 :: Int)
  let edges_to :: V.Vector IntSet.IntSet
      edges_to = V.create $ do
        sets <- VM.replicate n IntSet.empty
        U.forM_ edges $ \(x,y) -> do
          VM.modify sets (IntSet.insert x) y
        return sets
  let edges_from :: V.Vector IntSet.IntSet
      edges_from = V.create $ do
        sets <- VM.replicate n IntSet.empty
        U.forM_ edges $ \(x,y) -> do
          VM.modify sets (IntSet.insert y) x
        return sets
  let roots :: [Int]
      roots = filter (\i -> IntSet.null (edges_to V.! i)) [0..n-1]
  let loop :: Int -> IntSet.IntSet -> Int
      loop !k !set | IntSet.null set = k
      loop !k !set = loop (k+1) $ IntSet.unions $ map (\i -> edges_from V.! i) $ IntSet.toList set
      rootSet = IntSet.fromDistinctAscList roots
  print $ loop (-1) rootSet
