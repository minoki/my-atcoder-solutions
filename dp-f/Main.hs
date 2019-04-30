{-# LANGUAGE BangPatterns #-}
import Control.Monad
import Data.Word
import qualified Data.ByteString as BSW
import qualified Data.ByteString.Char8 as BS
import Data.Array.Unboxed
import Data.Array.ST
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

-- Input: s t
-- Output: arr
--   arr ! (i,j) == length of lcs of (drop i s, drop j t)
lcsTable2 :: BS.ByteString -> BS.ByteString -> U.Vector Word16
lcsTable2 s t = U.create $ do
  let !m = BS.length s
      !n = BS.length t
      !n' = n+1
  arr <- UM.replicate ((m+1) * n') 0
  forM_ [m-1,m-2..0] $ \ !i -> do
    let !x = BSW.index s i
        loopY !j !v
          -- v = UM.read arr $ i*n'+(j+1)
          | j < 0 = return ()
          | otherwise = do
              let !y = BSW.index t j
              if x == y
                then do l2 <- UM.read arr $ (i+1)*n'+(j+1)
                        let !l = l2 + 1
                        UM.write arr (i*n'+j) l
                        loopY (j-1) l
                else do l0 <- UM.read arr $ (i+1)*n'+j
                        let !l = max l0 v
                        UM.write arr (i*n'+j) l
                        loopY (j-1) l
    loopY (n-1) 0
  return arr

main = do
  s <- BS.getLine
  t <- BS.getLine
  -- BS.length s <= 3000, BS.length t <= 3000, BS.all isAsciiLower s, BS.all isAsciiLower t
  let !table = lcsTable2 s t
      !m = BS.length s
      !n = BS.length t
      !n' = n+1
  let recon !i !j | i >= m || j >= n = Nothing
                  | x == y = let !i' = i+1 ; !j' = j+1
                             in Just (x, (i', j'))
                  | table U.! ((i+1)*n'+j) >= table U.! (i*n'+(j+1)) = reconY y (i+1) j
                  | otherwise = reconX x i (j+1)
        where x = BSW.index s i
              y = BSW.index t j
      reconX !x !i !j | j >= n = Nothing
                      | x == y = let !i' = i+1 ; !j' = j+1
                                 in Just (x, (i', j'))
                      | table U.! ((i+1)*n'+j) >= table U.! (i*n'+(j+1)) = reconY y (i+1) j
                      | otherwise = reconX x i (j+1)
        where y = BSW.index t j
      reconY !y !i !j | i >= m = Nothing
                      | x == y = let !i' = i+1 ; !j' = j+1
                                 in Just (x, (i', j'))
                      | table U.! ((i+1)*n'+j) >= table U.! (i*n'+(j+1)) = reconY y (i+1) j
                      | otherwise = reconX x i (j+1)
        where x = BSW.index s i
      (result, _) = BSW.unfoldrN (fromIntegral $ table U.! 0) (\(!i,!j) -> recon i j) (0,0)
  BS.putStrLn result
