{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE PartialTypeSignatures #-}
-- {-# OPTIONS_GHC -Wno-partial-type-signatures #-}
import Control.Monad
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS
import Data.Array.ST
import Control.Monad.ST
import Data.STRef

asSTUArray :: ST s (STUArray s i a) -> ST s (STUArray s i a)
asSTUArray = id

main = do
  [h,w] <- map (read . BS.unpack) . BS.words <$> BS.getLine
  -- 1 <= h <= 1000, 1 <= w <= 1000
  initialState <- V.replicateM h BS.getLine
  -- for all (i, j), ((a ! i) `BS.index` j) `elem` "#."
  let initCells :: U.Vector (Int,Int) -- [(Int,Int)]
      initCells = U.fromList [(i,j) | i <- [0..h-1], j <- [0..w-1], (initialState V.! i) `BS.index` j == '#']
  let answer :: Int
      answer = runST $ do
        arr <- asSTUArray $ newArray ((0,0),(h-1,w-1)) False
        U.forM_ initCells $ \p ->
          writeArray arr p True
        let -- loop :: Int -> U.Vector (Int,Int) -> _ Int
            loop !k xs = do
              lenRef <- UM.replicate 1 0
              vec <- UM.new (4 * U.length xs)
              let -- push :: (Int,Int) -> _ ()
                  push value = do
                    len <- UM.read lenRef 0
                    UM.write vec len value
                    UM.write lenRef 0 (len + 1)
              let doCell !i !j
                    | 0 <= i && i < h && 0 <= j && j < w = do
                        b <- readArray arr (i,j)
                        when (not b) $ do
                          writeArray arr (i,j) True
                          push (i,j)
                    | otherwise = return ()
              U.forM_ xs $ \(i,j) -> do
                doCell (i-1) j
                doCell (i+1) j
                doCell i (j-1)
                doCell i (j+1)
              len <- UM.read lenRef 0
              if len == 0
                then return k
                else do ys <- U.take len <$> U.unsafeFreeze vec
                        loop (k+1) ys
        loop 0 initCells
  print answer
