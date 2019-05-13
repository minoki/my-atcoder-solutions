{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as BS

main = do
  n <- readLn
  strings <- V.replicateM n BS.getLine
  let breakAB = BS.breakSubstring "AB"
      findAB_loop !n s | BS.null s = n
                       | otherwise = case breakAB s of
                           (t,u) | BS.null u -> n
                                 | otherwise -> findAB_loop (n+1) (BS.drop 2 u)
      findAB = findAB_loop 0
  let ab_in_str = V.sum (V.map findAB strings)
  let (ends_with_A, does_not_end_with_A) = V.partition (\s -> BS.last s == 'A') strings
      (b_a, x_a) = V.partition (\s -> BS.head s == 'B') ends_with_A
      (b_y, x_y) = V.partition (\s -> BS.head s == 'B') does_not_end_with_A
      n_BA = V.length b_a
      n_xA = V.length x_a
      n_By = V.length b_y
  let m = case (n_xA, n_By) of
        (0, 0) -> max 0 (n_BA - 1)
        (_, 0) -> n_BA
        (0, _) -> n_BA
        (_, _) -> min n_xA n_By + n_BA
  print (ab_in_str + m)
