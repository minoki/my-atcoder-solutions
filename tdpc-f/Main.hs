import Control.Monad
import Data.Int
import Data.List
import Data.Array.IO

modulo :: Int64
modulo = 10^9 + 7
addMod x y = (x + y) `rem` modulo
subMod x y = (x - y) `mod` modulo
sumMod = foldl' addMod 0

parseInts :: String -> [Int]
parseInts s = case reads s of
                [(x,t)] -> x : parseInts t
                _ -> []

main = do
  [n,k] <- parseInts <$> getLine
  -- 2 <= k <= n <= 10^6
  arr <- newArray ((0, 0), (n - 1, k - 2)) 0 :: IO (IOUArray (Int,Int) Int64)
  forM_ [0..k-2] $ \j -> do
    writeArray arr (0, j) 1
  forM_ [1..n-1] $ \i -> do
    s <- sumMod <$> sequence [readArray arr (i', 0) | i' <- [0..i-2]]
    forM_ [0..k-3] $ \j -> do
      t <- readArray arr (i-1, j+1)
      writeArray arr (i, j) (s `addMod` t)
    writeArray arr (i, k-2) s
  ans <- readArray arr (n-1, 0)
  print ans
