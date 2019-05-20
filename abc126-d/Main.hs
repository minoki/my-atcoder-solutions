{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Data.Char
import Data.List
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS
import qualified Data.IntMap as IntMap
import Control.Monad

type Weight = Int

main = do
  n <- readLn
  edges <- U.replicateM (n-1) $ do
    [u,v,w] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    return (u, v, w)
  let tree :: IntMap.IntMap (IntMap.IntMap Weight)
      tree = U.foldl' (\m (u, v, w) -> IntMap.insertWith IntMap.union (u-1) (IntMap.singleton (v-1) w) $ IntMap.insertWith IntMap.union (v-1) (IntMap.singleton (u-1) w) m) IntMap.empty edges
  let result = U.create $ do
        vec <- UM.replicate n (-1)
        -- white: 0, black: 1
        let traverse !i !color = do
              x <- UM.read vec i
              if x == -1
                then do UM.write vec i color
                        let Just t = IntMap.lookup i tree
                        forM_ (IntMap.toList t) $ \(j,w) -> do
                          traverse j ((color + w) `rem` 2)
                else return ()
        traverse 0 0
        return vec
  U.forM_ result $ print
