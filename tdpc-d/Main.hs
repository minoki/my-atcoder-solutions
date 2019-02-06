{-# LANGUAGE BangPatterns #-}
import Data.Int
import qualified Data.Map.Strict as M

-- 10^18 < 9 * 10^18 < 2^63

type Map = M.Map Int64 Double

step :: Map -> Map
step !m = M.fromListWith (+)
          [ (d', v')
          | i <- [1..6]
          , (d,v) <- M.toList m
          , let !d' = d `quot` gcd i d
                !v' = v / 6
          ]

main = do
  l <- getLine
  let [(n,l')] = reads l :: [(Int,String)] -- 1 <= n <= 100
      [(d,_)] = reads l' :: [(Int64,String)] -- 1 <= d <= 10^18
  if d == 1
    then print 1.0
    else do
    let m = iterate step (M.singleton d 1.0) !! n
    case M.lookup 1 m of
      Just p -> print p
      Nothing -> print 0.0
