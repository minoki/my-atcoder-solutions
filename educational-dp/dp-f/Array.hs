{-# LANGUAGE BangPatterns #-}
import qualified Data.ByteString.Char8 as BS
import Data.Array

-- Input: s t
-- Output: arr
--   arr!(i,j) == length of lcs of (take i s, take j t)
lcsTable :: BS.ByteString -> BS.ByteString -> Array (Int, Int) Int
lcsTable xs ys  = let arr :: Array (Int, Int) Int
                      arr = array ((0,0), (n,m)) $
                        [ ((i,0),0) | i <- [0..n] ] ++
                        [ ((0,j),0) | j <- [1..m] ] ++
                        [ ((i1,j1),a) | i <- [0..n-1]
                                      , let !x = BS.index xs i
                                      , j <- [0..m-1]
                                      , let !y = BS.index ys j
                                      , let !i1 = i+1; !j1 = j+1
                                      , let a | x == y = 1 + arr!(i,j)
                                              | otherwise = max (arr!(i1,j)) (arr!(i,j1))
                                      ]
               in arr
  where !n = BS.length xs
        !m = BS.length ys

solve :: BS.ByteString -> BS.ByteString -> String
solve !xs !ys = let !table = lcsTable xs ys
                    !n = BS.length xs
                    !m = BS.length ys
                    recon !i !j acc | i == 0 || j == 0 = acc
                                    | x == y = recon (i-1) (j-1) (x : acc)
                                    | table!(i-1,j) >= table!(i,j-1) = recon (i-1) j acc
                                    | otherwise = recon i (j-1) acc
                      where x = BS.index xs (i-1)
                            y = BS.index ys (j-1)
                in recon n m []

main = do
  xs <- BS.getLine
  ys <- BS.getLine
  -- BS.length s <= 3000, BS.length t <= 3000, BS.all isAsciiLower s, BS.all isAsciiLower t
  putStrLn $ solve xs ys
