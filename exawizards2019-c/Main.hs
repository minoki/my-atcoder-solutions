{-# LANGUAGE BangPatterns #-}
import Control.Monad
import Data.Int
import Data.Char
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS
import qualified Data.IntSet as IntSet
import Data.IORef

main = do
  [n,q] <- map (read . BS.unpack) . BS.words <$> BS.getLine
  -- 1 <= n, q <= 2 * 10^5
  cellLabels <- BS.getLine -- length: n
  let charToCellIndexes :: V.Vector ([Int], [Int])
      charToCellIndexes = V.map (\s -> (IntSet.toAscList s, IntSet.toDescList s)) $ V.create $ do
        vec <- VM.replicate 26 IntSet.empty
        forM_ [0..BS.length cellLabels - 1] $ \i -> do
          let c = BS.index cellLabels i
          set <- VM.read vec (ord c - ord 'A')
          VM.write vec (ord c - ord 'A') $! IntSet.insert i set
          -- VM.modify vec (IntSet.insert i) (ord c - ord 'A')
        return vec
  -- print charToCellIndexes
  cells <- UM.replicate n (1 :: Int)
  res <- newIORef (n :: Int)
  forM_ [1..q] $ \_ -> do
    [t,' ',d] <- getLine
    case d of
      'L' -> forM_ (fst $ charToCellIndexes V.! (ord t - ord 'A')) $ \i -> do
        k <- UM.read cells i
        if i == 0
          then modifyIORef' res (subtract k)
          else UM.modify cells (+ k) (i - 1)
        UM.write cells i 0
      'R' -> forM_ (snd $ charToCellIndexes V.! (ord t - ord 'A')) $ \i -> do
        k <- UM.read cells i
        if i == n - 1
          then modifyIORef' res (subtract k)
          else UM.modify cells (+ k) (i + 1)
        UM.write cells i 0
  -- forM_ [0..n-1] $ \i -> do
  --   print =<< UM.read cells i
  result <- readIORef res
  -- result <- sum <$> mapM (UM.read cells) [0..n-1]
  print result
