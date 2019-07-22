{-# LANGUAGE BangPatterns #-}
import Data.Char
import Data.List
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS
import qualified Data.IntSet as IntSet
import qualified Data.IntMap.Strict as IntMap
import Control.Monad

main = do
  [n,m] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  edges <- U.replicateM m $ do
    [x,y,z] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    return (x-1,y-1,z)
  let tree :: IntMap.IntMap IntSet.IntSet
      tree = U.foldl' (\m (u, v, _) -> IntMap.insertWith IntSet.union u (IntSet.singleton v) $ IntMap.insertWith IntSet.union v (IntSet.singleton u) m) IntMap.empty edges
  vec <- UM.replicate n (-1)
  let find !j
        | j >= n = return Nothing
        | otherwise = do v <- UM.read vec j
                         if v == -1
                           then return (Just j)
                           else find (j+1)
  let dfs !i !value = do
        k <- UM.read vec i
        if k == -1
          then do UM.write vec i value
                  case IntMap.lookup i tree of
                    Just set -> forM_ (IntSet.toList set) $ \j -> dfs j value
                    Nothing -> return ()
          else return ()
  let loop !i = do
        mj <- find i
        case mj of
          Nothing -> return i
          Just j -> do
            dfs j i
            loop (i+1)
  result <- loop 0
  print result
