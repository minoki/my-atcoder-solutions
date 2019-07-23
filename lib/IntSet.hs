module IntSet where
import Data.Foldable (foldlM)
import Data.Monoid
import qualified Data.IntSet as IntSet
import Control.Monad

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
