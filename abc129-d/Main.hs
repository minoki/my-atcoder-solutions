-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as BS
import qualified Data.IntSet as IntSet

rowToSet :: BS.ByteString -> IntSet.IntSet
rowToSet s = snd $ BS.foldl' (\(i,set) c -> let !i' = i+1
                                                !set' | c == '#' = IntSet.insert i set
                                                      | otherwise = set
                                            in (i', set')
                             ) (0, IntSet.empty) s

colToSet :: V.Vector BS.ByteString -> Int -> IntSet.IntSet
colToSet v !j = V.ifoldl' (\set i s -> if BS.index s j == '#'
                                       then IntSet.insert i set
                                       else set
                          ) IntSet.empty v

main = do
  [h,w] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  grid <- V.replicateM h BS.getLine
  let rowSets = V.map rowToSet grid
      colSets = V.map (colToSet grid) $ V.enumFromN 0 w
  let result = maximum [ r1 - r0 + c1 - c0 - 3
                       | i <- [0..h-1]
                       , j <- [0..w-1]
                       , let r0 = fromMaybe (-1) (IntSet.lookupLE j (rowSets V.! i))
                       , r0 /= j
                       , let c0 = fromMaybe (-1) (IntSet.lookupLE i (colSets V.! j))
                       , c0 /= i
                       , let r1 = fromMaybe w (IntSet.lookupGT j (rowSets V.! i))
                       , let c1 = fromMaybe h (IntSet.lookupGT i (colSets V.! j))
                       ]
  print result
