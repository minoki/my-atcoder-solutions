-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.List (unfoldr)
import Control.Monad
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Control.Monad.ST

main = do
  n <- readLn
  points <- U.replicateM n $ do
    [x,y] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    return (x,y)
  let b = 1 + (U.maximum $ U.map fst points)
  let m0 :: IntMap.IntMap IntSet.IntSet
      m0 = U.foldr' (\(x,y) -> IntMap.insertWith IntSet.union x (IntSet.singleton y)) IntMap.empty points
      m1 :: IntMap.IntMap IntSet.IntSet
      m1 = U.foldr' (\(x,y) -> IntMap.insertWith IntSet.union y (IntSet.singleton x)) IntMap.empty points
      resultR :: U.Vector Int
      resultS :: V.Vector IntSet.IntSet
      resultN :: U.Vector Int
      (resultR, resultS, resultN) = runST $ do
        root <- U.thaw $ U.enumFromN 0 b
        ss <- VM.replicate b IntSet.empty
        forM_ (IntMap.assocs m0) $ \(k,v) -> VM.write ss k v
        numberOfElements <- UM.replicate b 1
        let getRoot !i = do
              !j <- UM.read root i
              if i == j
                then return i
                else do k <- getRoot j
                        UM.write root i k
                        return k
            unify !i !j = do
              !i' <- getRoot i -- []
              !j' <- getRoot j -- []
              if i' == j'
                then return ()
                else do
                let !k = min i' j'
                UM.write root i' k
                UM.write root j' k
                s1 <- VM.read ss i'
                s2 <- VM.read ss j'
                VM.write ss k $! IntSet.union s1 s2
                n1 <- UM.read numberOfElements i'
                n2 <- UM.read numberOfElements j'
                UM.write numberOfElements k (n1 + n2)
        forM_ (IntMap.elems m1) $ \t -> do
          let t0:ts = IntSet.toList t
          forM_ ts $ \j ->
            unify t0 j
        liftM3 (,,) (U.freeze root) (V.freeze ss) (U.freeze numberOfElements)
  let l = sum [IntSet.size (resultS V.! i) * resultN U.! i | i <- [0..b-1], resultR U.! i == i]
  print $ l - n
