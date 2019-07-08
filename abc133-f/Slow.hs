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
        parentMap <- UM.new n
        let go !i0 !i !d !color !dist = do
              UM.write depthVec i d
              UM.write parentMap i (i0,color,dist)
              forM_ (edges' V.! i) $ \(j,color,dist) -> do
                when (j /= i0) $ do
                  go i j (d+1) color dist
        go (-1) 0 0 (-1) 0
        liftM2 (,) (U.unsafeFreeze depthVec) (U.unsafeFreeze parentMap)
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
