-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import qualified Data.ByteString.Char8 as BS

splitBy :: Char -> BS.ByteString -> [BS.ByteString]
splitBy !c s = go 0
  where
    go !i | i == BS.length s = [s]
          | BS.index s i == c = BS.take i s : splitBy c (BS.drop (i+1) s)
          | otherwise = go (i+1)

nonZeroSpans :: [Int] -> [[Int]]
nonZeroSpans [] = []
nonZeroSpans (0:xs) = nonZeroSpans xs
nonZeroSpans xs = let (ys,zs) = span (/= 0) xs
                  in ys : nonZeroSpans zs

solveOne :: Int -> Int -> [Int] -> Int
solveOne !n !m [] = n + (m+1) `quot` 2
solveOne !n !m (x0:x1:xs)
  | x0 >= 1 && x1 >= 1 && x0+x1 >= 3 = let !x = x0+x1-2
                                       in solveOne (n+1) m (x:xs)
  | otherwise = solveOne n (m+1) (x1:xs)
solveOne !n !m [x] = if m <= x
                     then n + m
                     else n + (m-x) `quot` 2 + x

main = do
  s <- BS.getLine
  let xs = map BS.length $ splitBy 'w' s
  let result = sum $ map (solveOne 0 0) $ nonZeroSpans xs
  print result
