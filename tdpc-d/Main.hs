{-# LANGUAGE BangPatterns #-}
import Data.Int
import qualified Data.Map.Strict as M

-- 10^18 < 9 * 10^18 < 2^63

type Map = M.Map Int64 Double

step :: Int64 -> Map -> Map
step !d !m = M.fromListWith (+)
             [ (k', v')
             | i <- [1..6]
             , (k,v) <- M.toList m
             , let !k' = i * k `rem` d
                   !v' = v / 6
             ]

main = do
  l <- getLine
  let [(n,l')] = reads l :: [(Int,String)]
      [(d,_)] = reads l' :: [(Int64,String)]
  if d == 1
    then print 1.0
    else do
    let m = iterate (step d) (M.singleton 1 1.0) !! n
    case M.lookup 0 m of
      Just p -> print p
      Nothing -> print 0.0
