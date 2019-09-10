{-# LANGUAGE BangPatterns #-}
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

-- Input: s t
-- Output: arr
--   arr!(i,j) == length of lcs of (take i s, take j t)
lcsTable :: BS.ByteString -> BS.ByteString
         -> V.Vector (VU.Vector Int)
lcsTable xs ys = V.scanl step (VU.replicate (m+1) 0) xs'
  where
    n = BS.length xs; m = BS.length ys
    xs' = V.generate n $ BS.index xs :: V.Vector Char
    ys' = VU.generate m $ BS.index ys :: VU.Vector Char
    step :: VU.Vector Int -> Char -> VU.Vector Int
    step v x
      = VU.scanl innerStep 0 (VU.zip3 v (VU.tail v) ys')
      where
        innerStep :: Int -> (Int, Int, Char) -> Int
        innerStep a (b,c,y) | x == y    = 1 + b
                            | otherwise = max a c

solve :: BS.ByteString -> BS.ByteString -> String
solve !s !t = let !table = lcsTable s t
                  !n = BS.length s
                  !m = BS.length t
                  recon !i !j acc | i == 0 || j == 0 = acc
                                  | x == y = recon (i-1) (j-1) (x : acc)
                                  | table V.! (i-1) VU.! j >= table V.! i VU.! (j-1) = recon (i-1) j acc
                                  | otherwise = recon i (j-1) acc
                    where x = BS.index s (i-1)
                          y = BS.index t (j-1)
              in recon n m []

main = do
  s <- BS.getLine
  t <- BS.getLine
  -- BS.length s <= 3000, BS.length t <= 3000, BS.all isAsciiLower s, BS.all isAsciiLower t
  putStrLn (solve s t)
