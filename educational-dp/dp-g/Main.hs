-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Control.Monad
import Data.Char
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS
import qualified Data.IntSet as IntSet
-- import qualified Data.IntSet.Internal
import Data.Monoid hiding ((<>))
#ifdef MIN_VERSION_base
#if MIN_VERSION_base(4,9,0)
import Data.Semigroup
#endif
#endif
import Data.Foldable

forM_IntSet :: Monad m => IntSet.IntSet -> (Int -> m ()) -> m ()
forM_IntSet set f = go set
  where
    go set = case IntSet.splitRoot set of
               [] -> return ()
               [x] -> forM_ (IntSet.toList x) f
               xs -> forM_ xs go

-- n: commutative monoid
foldMap_IntSet :: (Monoid n) => (Int -> n) -> IntSet.IntSet -> n
foldMap_IntSet f set = go set
  where
    go set = case IntSet.splitRoot set of
               [] -> mempty
               [x] -> foldMap f (IntSet.toList x)
               xs -> foldMap go xs

-- n: commutative monoid, m: commutative monad
foldMapM_IntSet :: (Monoid n, Monad m) => (Int -> m n) -> IntSet.IntSet -> m n
foldMapM_IntSet f set = go set
  where
    go set = case IntSet.splitRoot set of
               [] -> return mempty
               [x] -> foldlM (\x v -> mappend x <$> f v) mempty (IntSet.toList x)
               xs -> foldlM (\x set' -> mappend x <$> go set') mempty xs

topologicalSort :: V.Vector IntSet.IntSet -> U.Vector Int
topologicalSort edges_from = U.create $ do
  let !n = V.length edges_from -- 頂点の個数
  vec <- UM.new n
  seen <- UM.replicate n False
  iref <- UM.replicate 1 (n - 1)
  let dfs x = do
        UM.write seen x True
        forM_IntSet (edges_from V.! x) $ \y -> do
          -- 辺 (x,y) が存在
          s <- UM.read seen y
          unless s $ dfs y
        i <- UM.read iref 0
        UM.write vec i x
        UM.write iref 0 (i - 1)
  forM_ [0..n-1] $ \x -> do
    s <- UM.read seen x
    unless s $ dfs x
  return vec

-- 非負整数に関する Max モノイド
newtype NNInt_Max = NNInt_Max { getMax_NNInt :: Int }
#ifdef MIN_VERSION_base
#if MIN_VERSION_base(4,9,0)
instance Semigroup NNInt_Max where
  NNInt_Max x <> NNInt_Max y = NNInt_Max (max x y)
#endif
#endif
instance Monoid NNInt_Max where
  mempty = NNInt_Max 0
  NNInt_Max x `mappend` NNInt_Max y = NNInt_Max (max x y)

readIntPair :: BS.ByteString -> (Int, Int)
readIntPair s = let Just (a, s') = BS.readInt s
                    Just (b, _) = BS.readInt $ BS.dropWhile isSpace s'
                in (a, b)

main = do
  (n,m) <- readIntPair <$> BS.getLine
  -- 2 <= n <= 10^5, 1 <= m <= 10^5
  edges :: U.Vector (Int, Int) <- U.replicateM m $ do
    (x,y) <- readIntPair <$> BS.getLine
    -- 1 <= x, y <= n
    return (x - 1, y - 1 :: Int)
  let -- edges_to ! y : 辺 (x,y) が存在するような x の集合
      edges_to :: V.Vector IntSet.IntSet
      edges_to = V.create $ do
        sets <- VM.replicate n IntSet.empty
        U.forM_ edges $ \(x,y) -> do
          VM.modify sets (IntSet.insert x) y
        return sets
      -- edges_from ! x : 辺 (x,y) が存在するような y の集合
      edges_from :: V.Vector IntSet.IntSet
      edges_from = V.create $ do
        sets <- VM.replicate n IntSet.empty
        U.forM_ edges $ \(x,y) -> do
          VM.modify sets (IntSet.insert y) x
        return sets
      -- 頂点の番号をトポロジカルソートしたやつ
      sorted :: U.Vector Int
      sorted = topologicalSort edges_from
  let result :: U.Vector Int
      result = U.create $ do
        vec <- UM.new n
        U.forM_ sorted $ \x -> do
          m <- foldMapM_IntSet (\y -> do t <- UM.read vec y
                                         return $! NNInt_Max (t + 1)) (edges_to V.! x)
          UM.write vec x (getMax_NNInt m)
        return vec
  print (U.maximum result)
