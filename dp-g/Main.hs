{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Control.Monad
import Data.Int
import Data.Bits
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS
import qualified Data.IntSet as IntSet
-- import qualified Data.IntSet.Internal
import Data.Monoid hiding ((<>))
import Data.Semigroup
import Data.Foldable

{-
modifyVector' :: VM.STVector s a -> (a -> a) -> Int -> ST s ()
modifyVector' vec f i = do
  x <- VM.read vec i
  VM.write vec i $! f x
-}

{-
extractIntSetTip :: IntSet.IntSet -> Maybe [Int]
extractIntSetTip set@(Data.IntSet.Internal.Tip prefix bitmap) =
  let loop 0 = []
      loop x = let !y = prefix .|. countTrailingZeros x in y : loop (x .&. (x - 1))
  in Just $ loop bitmap
extractIntSetTip _ = Nothing

forM_IntSet :: Monad m => IntSet.IntSet -> (Int -> m ()) -> m ()
forM_IntSet set f = go set
  where
    go tip@(Data.IntSet.Internal.Tip prefix bitmap) =
      let loop 0 = return ()
          loop x = do
            f $! (prefix .|. countTrailingZeros x)
            loop (x .&. (x - 1))
      in loop bitmap
    go set = do
      forM_ (IntSet.splitRoot set) go
-}
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

topologicalSort :: V.Vector IntSet.IntSet -> V.Vector IntSet.IntSet -> U.Vector Int
topologicalSort !edges_from edges_to = U.create $ do
  let !n = V.length edges_from
  vec <- UM.new n
  edges_to' <- V.thaw edges_to
  let go !i seen !unseen
        | i >= n = return ()
        | otherwise = do
            xs <- filterM (\x -> do
                              set <- VM.read edges_to' x
                              return (IntSet.null set {- && x `IntSet.notMember` seen -})) (IntSet.toList unseen)
            let innerLoop !i (!x:xs) seen unseen = do
                  UM.write vec i x
                  forM_IntSet (edges_from V.! x) $ \i -> do
                    set <- VM.read edges_to' i
                    VM.write edges_to' i $! IntSet.delete x set
                  innerLoop (i+1) xs (IntSet.insert x seen) (IntSet.delete x unseen)
                innerLoop !i [] seen unseen = go i seen unseen
            innerLoop i xs seen unseen
  go 0 IntSet.empty (IntSet.fromDistinctAscList [0..n-1])
  return vec

newtype NNInt_Max = NNInt_Max { getMax_NNInt :: Int }
instance Semigroup NNInt_Max where
  NNInt_Max x <> NNInt_Max y = NNInt_Max (max x y)
instance Monoid NNInt_Max where
  mempty = NNInt_Max 0
  NNInt_Max x `mappend` NNInt_Max y = NNInt_Max (max x y)

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
      sorted = topologicalSort edges_from edges_to
  let result :: U.Vector Int
      result = U.create $ do
        vec <- UM.new n
        U.forM_ sorted $ \x -> do
          -- m <- foldM (\ !m y -> do t <- UM.read vec y
          --                          return $! max m (t + 1)) 0 (IntSet.toList (edges_to V.! x))
          m <- foldMapM_IntSet (\y -> do t <- UM.read vec y
                                         return $! NNInt_Max (t + 1)) (edges_to V.! x)
          UM.write vec x (getMax_NNInt m)
        return vec
  -- print sorted
  -- print result
  print (U.maximum result)
