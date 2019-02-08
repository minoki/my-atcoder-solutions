{-# LANGUAGE BangPatterns #-}
import Control.Monad
import Data.Int
import Data.List
import Data.Array.IO
import qualified Data.Vector.Unboxed.Mutable as VM

modulo :: Int64
modulo = 10^9 + 7
addMod x y = (x + y) `rem` modulo
subMod x y = (x - y) `mod` modulo
sumMod :: [Int64] -> Int64
sumMod = foldl' addMod 0

parseInts :: String -> [Int]
parseInts s = case reads s of
                [(x,t)] -> x : parseInts t
                _ -> []

main = do
  [n,k] <- parseInts <$> getLine
  -- 2 <= k <= n <= 10^6
  v <- VM.new n :: IO (VM.IOVector Int64)
  -- t = T[i-1]
  let loop :: Int -> Int64 -> IO Int64
      loop !i !t | i == n = return t
                 | otherwise = do
                     t'' <- sumMod <$> sequence [VM.read v j | j <- [max 0 (i-k+1)..i-1]]
                     let t' = if i < k-1 then t'' + 1 else t''
                     s <- VM.read v (i-1)
                     VM.write v i (s `addMod` t)
                     loop (i+1) t'
  ans <- loop 1 1
  print ans
