-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.List (unfoldr)
import Data.Monoid
import Control.Monad
import Control.Monad.ST
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Sequence as Seq

strictAppend :: (Monoid a, Monoid b) => (a, b) -> (a, b) -> (a, b)
strictAppend (x, y) (x', y') = let !xx = x <> x'
                                   !yy = y <> y'
                               in (xx, yy)

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
  let routes :: V.Vector (Seq.Seq Int)
      distances :: U.Vector Int
      distancesMap :: V.Vector (IntMap.IntMap (Sum Int, Sum Int))
      (routes, distances, distancesMap) = runST $ do
        routes <- VM.new n
        distances <- UM.new n
        distancesMap <- VM.new n
        let go !i0 !i route !dt dtm = do
              VM.write routes i route
              UM.write distances i dt
              VM.write distancesMap i dtm
              forM_ (edges' V.! i) $ \(j,col,dist) -> do
                when (j /= i0) $ do
                  go i j (route Seq.|> j) (dt + dist) (IntMap.insertWith strictAppend col (Sum 1, Sum dist) dtm)
        go (-1) 0 (Seq.singleton 0) 0 IntMap.empty
        liftM3 (,,) (V.unsafeFreeze routes) (U.unsafeFreeze distances) (V.unsafeFreeze distancesMap)
      lowestCommonAncestor :: Int -> Int -> Int
      lowestCommonAncestor !i !j =
        let ri = routes V.! i
            rj = routes V.! j
            search !l !u | l + 1 == u = ri `Seq.index` l
            search !l !u = let d = (l + u) `quot` 2 -- l < d < u
                               pi = ri `Seq.index` d
                               pj = rj `Seq.index` d
                           in if pi == pj
                              then search d u
                              else search l d
        in search 0 (min (Seq.length ri) (Seq.length rj))
  forM_ [0..q-1] $ \_ -> do
    [x,y,up1,vp1] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    let u = up1 - 1
        v = vp1 - 1
        w = lowestCommonAncestor u v
        modifiedDistanceFromRoot :: Int -> Int
        modifiedDistanceFromRoot !i = distances U.! i + case IntMap.lookup x (distancesMap V.! i) of
                                                          Just (Sum n, Sum origDist) -> y * n - origDist
                                                          Nothing -> 0
    print $ modifiedDistanceFromRoot u + modifiedDistanceFromRoot v - 2 * modifiedDistanceFromRoot w
