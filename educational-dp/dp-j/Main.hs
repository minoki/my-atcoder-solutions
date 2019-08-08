{-# LANGUAGE BangPatterns #-}
import Data.Char
import Data.List
import Control.Monad
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Char8 as BS
import Control.Monad.ST
import Data.Array.ST

asSTUArray :: ST s (STUArray s i a) -> ST s (STUArray s i a)
asSTUArray arr = arr

solve :: Int -> Int -> Int -> Int -> Double
solve !n !b !c !d = runST $ do
  arr <- asSTUArray $ newArray ((-1,-1,-1),(n+1,n+1,n+1)) 0
  writeArray arr (0,0,0) 0
  forM_ [0..d] $ \k -> do
    forM_ [0..c+d-k] $ \j -> do
      -- j + k <= c + d
      forM_ [0..b+c+d-j-k] $ \i -> do
        -- i + j + k <= b + c + d
        when (i + j + k > 0) $ do
          x <- readArray arr (i-1,j,k)
          y <- readArray arr (i+1,j-1,k)
          z <- readArray arr (i,j+1,k-1)
          let t = fromIntegral (i + j + k)
          writeArray arr (i,j,k) $! (fromIntegral n + fromIntegral i * x + fromIntegral j * y + fromIntegral k * z) / t
  readArray arr (b,c,d)

main = do
  n <- readLn
  -- 1 <= n <= 300
  xs <- U.unfoldrN n (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  -- 1 <= ai <= 3
  let c1 = U.length (U.filter (== 1) xs)
      c2 = U.length (U.filter (== 2) xs)
      c3 = U.length (U.filter (== 3) xs)
  -- c1 + c2 + c3 should be n
  print $ solve n c1 c2 c3
