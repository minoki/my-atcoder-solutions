-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS
import qualified Data.IntMap as IntMap
import Control.Monad

type Weight = Int
type Tree = IntMap.IntMap [(Int,Weight)]

insert' :: Int -> a -> IntMap.IntMap [a] -> IntMap.IntMap [a]
insert' !k v = IntMap.alter (\m -> Just (v : fromMaybe [] m)) k

main = do
  n <- readLn
  edges <- U.replicateM (n-1) $ do
    [u,v,w] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    return (u-1, v-1, w)
  let tree :: Tree
      tree = U.foldl' (\m (u,v,w) -> insert' u (v,w) $ insert' v (u,w) m) IntMap.empty edges
  let result = U.create $ do
        vec <- UM.replicate n (-1)
        -- white: 0, black: 1
        let dfs !i !color = do
              x <- UM.read vec i
              when (x == -1) $ do
                UM.write vec i color
                let Just t = IntMap.lookup i tree
                forM_ t $ \(j,w) -> do
                  dfs j ((color + w) `rem` 2)
        dfs 0 0
        return vec
  U.mapM_ print result
