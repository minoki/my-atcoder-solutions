{-# LANGUAGE BangPatterns #-}
import Data.Word
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

-- Input: s t
-- Output: arr
--   (arr ! i) ! j == length of lcs of (drop i s, drop j t)
lcsTable :: BS.ByteString -> BS.ByteString -> V.Vector (U.Vector Word16)
lcsTable s t = V.scanr' (\ !x !v ->
                           -- for some i (0 <= i < m),
                           --   x = BS.index s i :: Char
                           --   v = arr ! (i+1) :: U.Vector Word16
                           U.scanr (\(!y,!w,!u) !l ->
                                      -- for some j (0 <= j < n)
                                      --   y = BS.index t j :: Char
                                      --   w = (arr ! (i+1)) ! j :: Word16
                                      --   u = (arr ! (i+1)) ! (j+1) :: Word16
                                      --   l = (arr ! i) ! (j+1) :: Word16
                                      if x == y
                                      then u + 1
                                      else max w l
                                   ) 0 (U.zip3 t' (U.init v) (U.tail v))
                        ) (U.replicate (n+1) 0) (V.fromListN m $ BS.unpack s)
  where !m = BS.length s
        !n = BS.length t
        !t' = U.fromListN n (BS.unpack t)

solve :: BS.ByteString -> BS.ByteString -> BS.ByteString
solve !s !t = let !table = lcsTable s t
                  !m = BS.length s
                  !n = BS.length t
                  recon !i !j | i >= m || j >= n = Nothing
                              | x == y = let !i' = i+1 ; !j' = j+1
                                         in Just (x, (i', j'))
                              | (table V.! (i+1)) U.! j >= (table V.! i) U.! (j+1) = recon (i+1) j
                              | otherwise = recon i (j+1)
                    where x = BS.index s i
                          y = BS.index t j
                  (result, _) = BS.unfoldrN (fromIntegral $ (table V.! 0) U.! 0) (\(!i,!j) -> recon i j) (0,0)
              in result

main = do
  s <- BS.getLine
  t <- BS.getLine
  -- BS.length s <= 3000, BS.length t <= 3000, BS.all isAsciiLower s, BS.all isAsciiLower t
  BS.putStrLn (solve s t)
