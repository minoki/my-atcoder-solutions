-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS
import qualified Data.IntMap.Strict as IntMap
import Control.Monad
import Control.Monad.ST
import Data.STRef

type Weight = Int
type Tree = IntMap.IntMap (U.Vector (Int,Weight))

data GrowableVector s a = GV {-# UNPACK #-} !Int !(U.MVector s a)

buildTree :: U.Vector (Int,Int,Weight) -> IntMap.IntMap (U.Vector (Int,Weight))
buildTree edges = runST $ do
  let insert' key val m = do
        case IntMap.lookup key m of
          Nothing -> do
            vec <- UM.new 1
            UM.write vec 0 val
            ref <- newSTRef (GV 1 vec)
            return $ IntMap.insert key ref m
          Just ref -> do
            GV len vec <- readSTRef ref
            vec' <- if len < UM.length vec then return vec else UM.grow vec len
            UM.write vec' len val
            writeSTRef ref $ GV (len+1) vec'
            return m
      adjust ref = do
        GV len mvec <- readSTRef ref
        vec <- U.unsafeFreeze mvec
        return (U.take len vec)
  m <- U.foldM' (\m (u,v,w) -> insert' v (u,w) m >>= insert' u (v,w)) IntMap.empty edges
  mapM adjust m

main = do
  n <- readLn
  edges <- U.replicateM (n-1) $ do
    [u,v,w] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    return (u-1, v-1, w)
  let tree :: Tree
      tree = buildTree edges
  let result = U.create $ do
        vec <- UM.replicate n (-1)
        -- white: 0, black: 1
        let dfs !i !color = do
              x <- UM.read vec i
              when (x == -1) $ do
                UM.write vec i color
                let Just t = IntMap.lookup i tree
                U.forM_ t $ \(j,w) -> do
                  dfs j ((color + w) `rem` 2)
        dfs 0 0
        return vec
  U.mapM_ print result
