{-# LANGUAGE BangPatterns #-}
import Data.List
import Data.Bits
import qualified Data.ByteString.Char8 as BS

readInt s = case BS.readInt s of
              Just (x, _) -> x

solve :: Int -> Int -> Int -> Int -> Int
solve !a !b !c !x
  | 500 * a + 100 * b + 50 * c < x = 0
  | c == 0 && x `rem` 100 == 50 = 0
  | otherwise = sum [ nb1 - nb0 + 1
                    | na <- [0..a]
                    , let nb0 = max 0 ((x - 500 * na - 50 * c + 50) `div` 100)
                          nb1 = min b ((x - 500 * na) `div` 100)
                    , nb0 <= nb1
                    -- nb <- [0..b]
                    -- nc <- [0..c]
                    -- 500 * na + 100 * nb + 50 * nc == x
                    -- nc = (x - 500 * na - 100 * nb) / 50
                    -- 0 <= x - 500 * na - 100 * nb, x - 500 * na - 100 * nb <= 50 * c
                    -- (x - 500 * na - 50 * c) / 100 <= nb, nb <= (x - 500 * na) / 100, nb is an integer
                    -- max 0 ((x - 500 * na - 50 * c + 50) / 100) <= nb <= min b ((x - 500 * na) / 100)
                    ]

solveNaive :: Int -> Int -> Int -> Int -> Int
solveNaive !a !b !c !x
  | 500 * a + 100 * b + 50 * c < x = 0
  | c == 0 && x `rem` 100 == 50 = 0
  | otherwise = sum [ 1
                    | na <- [0..a]
                    , nb <- [0..b]
                    , nc <- [0..c]
                    , 500 * na + 100 * nb + 50 * nc == x
                    ]

main = do
  a <- readInt <$> BS.getLine
  b <- readInt <$> BS.getLine
  c <- readInt <$> BS.getLine
  x <- readInt <$> BS.getLine
  print $ solve a b c x
