-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.List (unfoldr)
import Control.Monad
import Control.Monad.ST
import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS
import qualified Data.IntSet as IntSet
import Data.Monoid
import Data.Foldable
import qualified Data.ByteString.Builder as BSB
import System.IO (stdout)

printEdge :: Int -> Int -> IO ()
-- printEdge !i !j = putStrLn $ show i ++ " " ++ show j
printEdge !i !j = BSB.hPutBuilder stdout $ BSB.intDec i <> BSB.char7 ' ' <> BSB.intDec j <> BSB.char7 '\n'

solve :: Int -> Int -> U.Vector (Int,Int) -> IO ()
solve !n !m !edges = do
  let graphS :: V.Vector IntSet.IntSet
      graphS = V.create $ do
        vec <- VM.replicate n IntSet.empty
        U.forM_ edges $ \(a,b) -> do
          modify'_MV vec (IntSet.insert b) a
          modify'_MV vec (IntSet.insert a) b
        return vec
  let treeParent :: U.Vector Int
      treeSorted :: U.Vector Int
      (treeParent, treeSorted) = runST $ do
        parentVec <- UM.replicate n (-1)
        sorted <- UM.new n
        iRef <- UM.replicate 1 n
        let pushfront x = do i <- UM.read iRef 0
                             UM.write sorted (i-1) x
                             UM.write iRef 0 (i-1)
        let dfs !i = do
              forM_IntSet (graphS V.! i) $ \j -> do
                p <- UM.read parentVec j
                when (p == -1) $ do
                  UM.write parentVec j i
                  dfs j
              pushfront i
        UM.write parentVec 0 0
        dfs 0
        liftM2 (,) (U.unsafeFreeze parentVec) (U.unsafeFreeze sorted)
  graphM <- V.thaw graphS
  U.forM_ (U.reverse treeSorted) $ \i -> do
    e <- VM.read graphM i
    if even (IntSet.size e)
      then do forM_IntSet e $ \j -> do
                printEdge (i+1) (j+1)
                modify'_MV graphM (IntSet.delete i) j
      else do let p = treeParent U.! i
              forM_IntSet (IntSet.delete p e) $ \j -> do
                printEdge (i+1) (j+1)
                modify'_MV graphM (IntSet.delete i) j

main = do
  [n,m] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  edges <- U.replicateM m $ do
    [a,b] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    return (a-1,b-1)
  if even m then solve n m edges else putStrLn "-1"

-- strict version of VM.modify
modify'_MV :: PrimMonad m => VM.MVector (PrimState m) a -> (a -> a) -> Int -> m ()
modify'_MV mvec f !i = do x <- VM.read mvec i
                          VM.write mvec i $! f x

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
