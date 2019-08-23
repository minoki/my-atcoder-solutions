-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.List (unfoldr)
import Control.Monad
import Control.Monad.ST
import Control.Monad.State.Strict
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS

main = do
  [n,m,p] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  edges <- U.replicateM m $ do
    [a,b,c] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    return (a-1,b-1,p-c)
  let edges_to :: V.Vector [Int]
      edges_to = V.create $ do
        vec <- VM.replicate n []
        U.forM_ edges $ \(a,b,_) -> do
          VM.modify vec (a :) b
        return vec
  let reachable_from_end :: U.Vector Bool
      reachable_from_end = U.create $ do
        vec <- UM.replicate n False
        let dfs !i = do
              b <- UM.read vec i
              unless b $ do
                UM.write vec i True
                forM_ (edges_to V.! i) dfs
        dfs (n-1)
        return vec
  let edges' = U.filter (\(a,b,c) -> reachable_from_end U.! b) edges
  let result :: Maybe (U.Vector Int)
      result = runST $ do
        d <- UM.replicate n maxBound
        UM.write d 0 0
        let loop !i
              | i >= n = return Nothing
              | otherwise = do
                  update <- execStateT (U.forM_ edges' $ \(a,b,c) -> do
                                           a' <- UM.read d a
                                           b' <- UM.read d b
                                           when (a' /= maxBound && b' > a' + c) $ do
                                             UM.write d b (a' + c)
                                             put True
                                       ) False
                  if update
                    then loop (i + 1)
                    else Just <$> U.unsafeFreeze d
        loop 0
  case result of
    Nothing -> putStrLn "-1"
    Just d -> print $ max 0 (negate (d U.! (n - 1)))
