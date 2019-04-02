{-# LANGUAGE BangPatterns #-}
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Array.Unboxed
import Data.Array.ST

-- Input: s t
-- Output: arr
--   arr ! (i,j) == length of lcs of (drop i s, drop j t)
lcsTable2 :: BS.ByteString -> BS.ByteString -> UArray (Int,Int) Int
lcsTable2 s t = runSTUArray $ do
  let !m = BS.length s
      !n = BS.length t
  arr <- newArray ((0,0),(m,n)) 0
  forM_ [m-1,m-2..0] $ \ !i -> do
    let !x = BS.index s i
    forM_ [n-1,n-2..0] $ \ !j -> do
      let !y = BS.index t j
      l <- if x == y
           then do l2 <- readArray arr (i+1, j+1)
                   return (l2 + 1)
           else do l0 <- readArray arr (i+1, j)
                   l1 <- readArray arr (i, j+1)
                   return (max l0 l1)
      writeArray arr (i,j) l
  return arr

main = do
  s <- BS.getLine
  t <- BS.getLine
  -- BS.length s <= 3000, BS.length t <= 3000, BS.all isAsciiLower s, BS.all isAsciiLower t
  let !table = lcsTable2 s t
      !m = BS.length s
      !n = BS.length t
      reconstruct !i !j | i >= m || j >= n = ""
                        | x == y = x : reconstruct (i+1) (j+1)
                        | table ! (i+1,j) >= table ! (i,j+1) = reconstruct (i+1) j
                        | otherwise = reconstruct i (j+1)
        where x = BS.index s i
              y = BS.index t j
  let result = reconstruct 0 0
  putStrLn result
