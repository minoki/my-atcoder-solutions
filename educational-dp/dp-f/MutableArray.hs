{-# LANGUAGE BangPatterns #-}
import qualified Data.ByteString.Char8 as BS
import Data.Array.Unboxed
-- import Data.Array
import Data.Array.ST
import Control.Monad

-- Input: s t
-- Output: arr
--   arr!(i,j) == length of lcs of (take i s, take j t)
lcsTable :: BS.ByteString -> BS.ByteString -> UArray (Int, Int) Int
lcsTable xs ys = runSTUArray $ do
  arr <- newArray ((0,0), (n,m)) 0
  forM_ [0..n-1] $ \i -> do
    forM_ [0..m-1] $ \j -> do
      if xs `BS.index` i == ys `BS.index` j then do
        a <- readArray arr (i, j)
        writeArray arr (i+1, j+1) $! a+1
      else do
        a <- readArray arr (i+1, j)
        b <- readArray arr (i, j+1)
        writeArray arr (i+1, j+1) $! max a b
  return arr
 where n = BS.length xs; m = BS.length ys

solve :: BS.ByteString -> BS.ByteString -> String
solve !s !t = let !table = lcsTable s t
                  !n = BS.length s
                  !m = BS.length t
                  recon !i !j acc | i == 0 || j == 0 = acc
                                  | x == y = recon (i-1) (j-1) (x : acc)
                                  | table!(i-1,j) >= table!(i,j-1) = recon (i-1) j acc
                                  | otherwise = recon i (j-1) acc
                    where x = BS.index s (i-1)
                          y = BS.index t (j-1)
              in recon n m []

main = do
  s <- BS.getLine
  t <- BS.getLine
  -- BS.length s <= 3000, BS.length t <= 3000, BS.all isAsciiLower s, BS.all isAsciiLower t
  putStrLn (solve s t)
