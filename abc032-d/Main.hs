{-# LANGUAGE BangPatterns #-}
import Control.Monad
import qualified Data.IntMap as M
import qualified Data.Vector.Unboxed as V
import Data.Int

parseInts :: String -> [Int]
parseInts s = case reads s of
                [(x,t)] -> x : parseInts t
                _ -> []

-- wi's are small
solveV :: Int -> [(Int,Int)] -> V.Vector Int -> V.Vector Int
solveV !maxW [] map = map
solveV !maxW ((!v,!w):xs) map = solveV maxW xs
  $ if w < V.length map
    then V.generate (min (w + V.length map) (maxW + 1))
         $ \i -> if i < w
                 then map V.! i
                 else if i < V.length map
                      then max (map V.! i) ((map V.! (i - w)) + v)
                      else (map V.! (i - w)) + v
    else let l = (min (w + V.length map) (maxW + 1))
         in if w < l
            then V.generate l
                 $ \i -> if i < V.length map
                         then map V.! i
                         else if i < w
                              then 0
                              else (map V.! (i - w)) + v
            else map

-- vi's are small
solveM :: Int -> [(Int,Int)] -> M.IntMap Int -> M.IntMap Int
solveM !maxW [] map = map
solveM !maxW ((!v,!w):xs) map = solveM maxW xs
  $ M.unionWith min map
  $ M.fromAscList [ (v' + v, ww)
                  | (v',w') <- M.toAscList map
                  , let !ww = w + w'
                  , ww <= maxW
                  ]

main = do
  [n,maxW] <- parseInts <$> getLine
  -- 1 <= n <= 200, 1 <= maxW <= 10^9
  xs <- replicateM n $ do
    [vi,wi] <- parseInts <$> getLine
    -- 1 <= vi <= 10^9
    -- 1 <= wi <= 10^9
    return (vi,wi)
  if all (\(_,wi) -> wi <= 1000) xs
    then let m = solveV maxW xs (V.singleton 0)
         in print (V.maximum m)
    else let m = solveM maxW xs (M.singleton 0 0)
         in print (fst $ M.findMax m)
