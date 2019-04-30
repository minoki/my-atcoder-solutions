{-# LANGUAGE BangPatterns #-}
import Control.Monad
import Data.Word
import qualified Data.ByteString as BSW
import qualified Data.ByteString.Char8 as BS
import Data.Array.Unboxed
import Data.Array.ST

-- Input: s t
-- Output: arr
--   arr ! (i,j) == length of lcs of (drop i s, drop j t)
lcsTable2 :: BS.ByteString -> BS.ByteString -> UArray (Int,Int) Word16
lcsTable2 s t = runSTUArray $ do
  let !m = BS.length s
      !n = BS.length t
  arr <- newArray ((0,0),(m,n)) 0
  forM_ [m-1,m-2..0] $ \ !i -> do
    let !x = BSW.index s i
    forM_ [n-1,n-2..0] $ \ !j -> do
      let !y = BSW.index t j
      if x == y
        then do l2 <- readArray arr (i+1, j+1)
                writeArray arr (i,j) (l2 + 1)
        else do l0 <- readArray arr (i+1, j)
                l1 <- readArray arr (i, j+1)
                writeArray arr (i,j) (max l0 l1)
  return arr

main = do
  s <- BS.getLine
  t <- BS.getLine
  -- BS.length s <= 3000, BS.length t <= 3000, BS.all isAsciiLower s, BS.all isAsciiLower t
  let !table = lcsTable2 s t
      !m = BS.length s
      !n = BS.length t
  let recon !i !j | i >= m || j >= n = Nothing
                  | x == y = let !i' = i+1 ; !j' = j+1
                             in Just (x, (i', j'))
                  | table ! (i+1,j) >= table ! (i,j+1) = reconY y (i+1) j
                  | otherwise = reconX x i (j+1)
        where x = BSW.index s i
              y = BSW.index t j
      reconX !x !i !j | j >= n = Nothing
                      | x == y = let !i' = i+1 ; !j' = j+1
                                 in Just (x, (i', j'))
                      | table ! (i+1,j) >= table ! (i,j+1) = reconY y (i+1) j
                      | otherwise = reconX x i (j+1)
        where y = BSW.index t j
      reconY !y !i !j | i >= m = Nothing
                      | x == y = let !i' = i+1 ; !j' = j+1
                                 in Just (x, (i', j'))
                      | table ! (i+1,j) >= table ! (i,j+1) = reconY y (i+1) j
                      | otherwise = reconX x i (j+1)
        where x = BSW.index s i
      (result, _) = BSW.unfoldrN (fromIntegral $ table ! (0,0)) (\(!i,!j) -> recon i j) (0,0)
  BS.putStrLn result
