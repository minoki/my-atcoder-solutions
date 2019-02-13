{-# LANGUAGE BangPatterns #-}
import Control.Monad
import qualified Data.IntMap.Strict as M
import Data.Int

parseInts :: String -> [Int]
parseInts s = case reads s of
                [(x,t)] -> x : parseInts t
                _ -> []

solve :: Int -> [(Int,Int)] -> M.IntMap Int -> M.IntMap Int
solve !maxW [] map = map
solve !maxW ((v,w):xs) map = solve maxW xs $ M.unionWith min map
  $ M.fromAscList [ (v' - v, ww)
                  | (v',w') <- M.toAscList map
                  , let !ww = w + w'
                  , ww <= maxW
                  ]

main = do
  [n,w] <- parseInts <$> getLine
  -- 1 <= n <= 200, 1 <= w <= 10^9
  xs <- replicateM n $ do
    [vi,wi] <- parseInts <$> getLine
    -- 1 <= vi <= 10^9
    -- 1 <= wi <= 10^9
    return (vi,wi)
  let m = solve w xs (M.singleton 0 0)
      (v, _) = M.findMin m
  print (-v)
