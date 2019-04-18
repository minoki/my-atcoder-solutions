{-# LANGUAGE BangPatterns #-}
import Control.Monad
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as BS
import Data.Array.Unboxed
import Data.Array.IO
import qualified Data.Vector.Unboxed.Mutable as UM

main = do
  [h,w] <- map (read . BS.unpack) . BS.words <$> BS.getLine
  -- h, w <= 400
  ss <- V.replicateM h BS.getLine
  -- forall s in ss. BS.length s == w
  arr <- newArray ((0,0),(h-1,w-1)) (-1) :: IO (IOUArray (Int,Int) Int)
  num_b_w <- UM.new (h * w) :: IO (UM.IOVector (Int,Int))
  let search :: Int -> Int -> Int -> Int -> IO Int
      search !acc !k !i !j
        | i >= h = return acc
        | j >= w = search acc k (i+1) 0
        | otherwise = do
            v <- readArray arr (i,j)
            when (v == -1) $ do
              if (ss V.! i) `BS.index` j == '#'
                then search_black k i j -- black
                else search_white k i j
            (nb,nw) <- UM.read num_b_w k
            search (acc + nb * nw) (k+1) i (j+1)
      search_black :: Int -> Int -> Int -> IO ()
      search_black !k !i !j
        | i < 0 || j < 0 || i >= h || j >= w = return ()
        | (ss V.! i) `BS.index` j == '#' = do
            v <- readArray arr (i,j)
            when (v == -1) $ do
              writeArray arr (i,j) k
              UM.modify num_b_w (\(b,w) -> (b+1,w)) k
              search_white k (i-1) j
              search_white k i (j-1)
              search_white k (i+1) j
              search_white k i (j+1)
        | otherwise = return ()
      search_white :: Int -> Int -> Int -> IO ()
      search_white !k !i !j
        | i < 0 || j < 0 || i >= h || j >= w = return ()
        | (ss V.! i) `BS.index` j == '.' = do
            v <- readArray arr (i,j)
            when (v == -1) $ do
              writeArray arr (i,j) k
              UM.modify num_b_w (\(b,w) -> (b,w+1)) k
              search_black k (i-1) j
              search_black k i (j-1)
              search_black k (i+1) j
              search_black k i (j+1)
        | otherwise = return ()
  result <- search 0 0 0 0
  {-
  arr' <- freeze arr :: IO (UArray (Int,Int) Int)
  forM_ [0..h-1] $ \i -> do
    print [arr' ! (i,j) | j <- [0..w-1]]
  -}
  print result
