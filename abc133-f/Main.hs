-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.List (unfoldr)
import Control.Monad
import Control.Monad.ST
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS
import qualified Data.IntMap.Strict as IntMap

main = do
  [n,q] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  edges <- U.replicateM (n-1) $ do
    [a,b,c,d] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    return (a-1,b-1,c,d)
  let edges' :: V.Vector [(Int,Int,Int)]
      edges' = V.create $ do
        vec <- VM.replicate n []
        U.forM_ edges $ \(i,j,c,d) -> do
          VM.modify vec ((j,c,d) :) i
          VM.modify vec ((i,c,d) :) j
        return vec
  let depthMap :: U.Vector Int
      parentMap :: U.Vector (Int,Int,Int)
      (depthMap, parentMap) = runST $ do
        depthVec <- UM.new n
        toParent <- UM.new n
        let go !i0 !i !d !col !dist = do
              UM.write depthVec i d
              UM.write toParent i (i0,col,dist)
              forM_ (edges' V.! i) $ \(j,col,dist) -> do
                when (j /= i0) $ do
                  go i j (d+1) col dist
        go (-1) 0 0 (-1) 0
        liftM2 (,) (U.unsafeFreeze depthVec) (U.unsafeFreeze toParent)
        {-
      commonRoot :: Int -> Int -> Int
      commonRoot u v | u == v = u
                     | otherwise =
        let ud = depthMap U.! u
            vd = depthMap U.! v
            fst3 (a,_,_) = a
        in case compare ud vd of
             LT -> commonRoot u (fst3 $ parentMap U.! v)
             EQ -> commonRoot (fst3 $ parentMap U.! u) (fst3 $ parentMap U.! v)
             GT -> commonRoot (fst3 $ parentMap U.! u) v
      distanceFromRoot :: U.Vector Int
      colorsOnPath :: V.Vector (IntMap.IntMap Int)
      (distanceFromRoot, colorsOnPath) = runST $ do
        dist <- UM.new n
        col <- VM.new n
        let go !i0 !i !d !c = do
              UM.write dist i d
              VM.write col i c
              forM_ (edges' V.! i) $ \(j,c',d') -> do
                when (j /= i0) $ do
                  go i j (d+d') (IntMap.insertWith (+) c' 1 c)
        go (-1) 0 0 IntMap.empty
        liftM2 (,) (U.unsafeFreeze dist) (V.unsafeFreeze col)
-}
  forM_ [0..q-1] $ \_ -> do
    [x,y,up1,vp1] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    let u = up1 - 1
        v = vp1 - 1
        modifiedDistance :: Int -> Int -> Int
        modifiedDistance !color !originalDistance | color == x = y
                                                  | otherwise = originalDistance
    let go :: Int -> Int -> Int -> Int
        go !i !j !acc
          | i == j = acc
          | otherwise =
              let di = depthMap U.! i
                  dj = depthMap U.! j
              in case compare di dj of
                   LT -> let (j',cj,dj) = parentMap U.! j
                         in go i j' (acc + modifiedDistance cj dj)
                   EQ -> let (i',ci,di) = parentMap U.! i
                             (j',cj,dj) = parentMap U.! j
                         in go i' j' (acc + modifiedDistance ci di + modifiedDistance cj dj)
                   GT -> let (i',ci,di) = parentMap U.! i
                         in go i' j (acc + modifiedDistance ci di)
    print (go u v 0)
