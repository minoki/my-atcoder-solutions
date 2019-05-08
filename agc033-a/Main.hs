{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as BS
import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad.ST
import Control.Monad.State.Strict

asSTUArray :: ST s (STUArray s i a) -> ST s (STUArray s i a)
asSTUArray = id

main = do
  [h,w] <- map (read . BS.unpack) . BS.words <$> BS.getLine
  -- 1 <= h <= 1000, 1 <= w <= 1000
  initialState <- V.replicateM h BS.getLine
  -- for all (i, j), ((a ! i) `BS.index` j) `elem` "#."
  let answer :: Int
      answer = runST $ do
        arr <- asSTUArray $ newArray ((0,0),(h-1,w-1)) False
        let initCells :: [(Int,Int)]
            initCells = [(i,j) | i <- [0..h-1], j <- [0..w-1], (initialState V.! i) `BS.index` j == '#']
        forM_ initCells $ \p ->
          writeArray arr p True
        let -- loop :: Int -> [(Int,Int)] -> m Int
            loop !k xs = do
              ys <- flip execStateT [] $ do
                let doCell p@(!i,!j) | 0 <= i && i < h && 0 <= j && j < w = do
                                         b <- lift $ readArray arr p
                                         when (not b) $ do
                                           lift $ writeArray arr p True
                                           modify' (p :)
                                     | otherwise = return ()
                forM_ xs $ \(i,j) -> do
                  doCell (i-1,j)
                  doCell (i+1,j)
                  doCell (i,j-1)
                  doCell (i,j+1)
              case ys of
                [] -> return k
                _ -> loop (k+1) ys
        loop 0 initCells
  print answer
