{-# LANGUAGE BangPatterns #-}
import Control.Monad
import Data.Int
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.ByteString.Char8 as BS
import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad.ST

asSTUArray :: ST s (STUArray s i x) -> ST s (STUArray s i x)
asSTUArray x = x

-- Input: s
-- Output: arr
--   forall i x. let j = arr!(i,x) in j == -1 || (s `BS.index` j == x && all (\k -> s `BS.index` k /= x) [i..j-1])
indexTable :: BS.ByteString -> UArray (Int,Char) Int
indexTable s = runSTUArray $ do
  let !n = BS.length s
  arr <- newArray ((0,'a'),(n - 1,'z')) (-1)
  t <- asSTUArray $ newArray ('a','z') (-1)
  forM_ [n-1,n-2..0] $ \i -> do
    let x = BS.index s i
    writeArray t x i
    forM_ ['a'..'z'] $ \y -> do
      j <- readArray t y
      writeArray arr (i,y) j
  return arr

-- Input: s t t_tbl
--   t_tbl == indexTable t
-- Output: arr
--   arr ! (i,j) == length of lcs of (drop i s, drop j t)
lcsTable :: BS.ByteString -> BS.ByteString -> UArray (Int,Char) Int -> UArray (Int,Int) Int
lcsTable s t t_tbl = runSTUArray $ do
  let !m = BS.length s
      !n = BS.length t
  arr <- newArray ((0,0),(m,n)) 0
  forM_ [m-1,m-2..0] $ \ !i -> do
    let !x = BS.index s i
    forM_ [n-1,n-2..0] $ \ !j -> do
      l0 <- readArray arr (i+1, j)
      let k = t_tbl ! (j,x)
      if k /= -1
        then do l1 <- readArray arr (i+1, k+1)
                writeArray arr (i,j) $ max l0 (l1 + 1)
        else writeArray arr (i,j) l0
  return arr

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
  let -- !t_tbl = indexTable t
      -- !table = lcsTable s t t_tbl
      !table = lcsTable2 s t
      !m = BS.length s
      !n = BS.length t
      reconstruct !i !j | i >= m || j >= n = ""
                        | x == y = x : reconstruct (i+1) (j+1)
                        | table ! (i+1,j) >= table ! (i,j+1) = reconstruct (i+1) j
                        | otherwise = reconstruct i (j+1)
        where x = BS.index s i
              y = BS.index t j
  let result = reconstruct 0 0
  if length result == table ! (0,0)
    then putStrLn result
    else error "length mismatch"
