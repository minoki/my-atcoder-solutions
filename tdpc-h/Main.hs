{-# LANGUAGE BangPatterns #-}
import Control.Monad
import Control.Monad.ST
import qualified Data.Map as M
import Data.Bits
import Data.Word
import qualified Data.Vector.Mutable as VM

parseInts :: String -> [Int]
parseInts s = case reads s of
                [(x,t)] -> x : parseInts t
                _ -> []

solve :: Int -> Int -> Word64 -> Int -> [(Int,Int,Int)] -> VM.STVector s (M.Map (Int,Word64) Int) -> ST s Int
solve !maxColors !maxWeight !colors !i [] memo = return 0
solve !maxColors 0 !colors !i _ memo = return 0
solve !maxColors !maxWeight !colors !i ((w,v,c):xs) memo
  | popCount colors == maxColors && not (testBit colors c) = solve maxColors maxWeight colors (i + 1) xs memo
  | w > maxWeight = solve maxColors maxWeight colors (i + 1) xs memo
  | otherwise = do
      map <- VM.read memo i
      case M.lookup (maxWeight, colors) map of
        Just u -> return u
        Nothing -> do
          a <- solve maxColors maxWeight colors (i + 1) xs memo
          b <- solve maxColors (maxWeight - w) (setBit colors c) (i + 1) xs memo
          let u = max a (b + v)
          VM.write memo i (M.insert (maxWeight, colors) u map)
          return u

main = do
  [n,w,c] <- parseInts <$> getLine
  -- 1 <= n <= 100, 1 <= w <= 10000, 1 <= c <= 50
  xs <- replicateM n $ do
    [wi,vi,ci] <- parseInts <$> getLine
    -- 1 <= wi, vi <= 10000
    -- 1 <= ci <= 50
    return (wi,vi,ci)
  let result = runST $ do memo <- VM.replicate (n + 1) M.empty
                          solve c w 0 0 xs memo
  print result
