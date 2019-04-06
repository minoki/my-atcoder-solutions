{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Control.Monad
import Data.Int (Int64)
import Data.List
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Char8 as BS
import Data.Ord
import Data.Coerce

sortDown :: forall a. (Ord a) => [a] -> [a]
sortDown = coerce . (sort :: [Down a] -> [Down a]) . coerce

mergeTwo :: [Int64] -> [Int64] -> [Int64]
mergeTwo xs@(x:xss) ys@(y:yss) = case compare x y of
  LT -> y : mergeTwo xs yss
  EQ -> x : y : mergeTwo xss yss
  GT -> x : mergeTwo xss ys
mergeTwo [] ys = ys
mergeTwo xs [] = xs

mergeLists :: [[Int64]] -> [Int64]
mergeLists [] = []
mergeLists (xs:sss) = doMerge' xs sss
  where
    doMerge' xs (ys:sss) = doMerge xs ys sss
    doMerge' xs [] = xs
    doMerge xs (y:yss) sss = innerLoop xs y yss sss
      where innerLoop xs@(x:xss) !y yss sss = case compare x y of
              LT -> y : doMerge' (mergeTwo xs yss) sss
              EQ -> x : y : doMerge' (mergeTwo xss yss) sss
              GT -> x : innerLoop xss y yss sss
            innerLoop [] y yss sss = y : doMerge' yss sss
    doMerge [] ys [] = ys
    doMerge [] ys (zs:sss) = doMerge ys zs sss
    doMerge xs [] (ys:sss) = doMerge xs ys sss
    doMerge xs [] [] = xs

main = do
  [x,y,z,k] :: [Int] <- map (read . BS.unpack) . BS.words <$> BS.getLine
  -- x,y,z <= 1000
  -- k <= min 3000 (x*y*z)
  as :: U.Vector Int64 <- U.fromListN x . sortDown . map (read . BS.unpack) . BS.words <$> BS.getLine
  bs :: U.Vector Int64 <- U.fromListN y . sortDown . map (read . BS.unpack) . BS.words <$> BS.getLine
  cs :: U.Vector Int64 <- U.fromListN z . sortDown . map (read . BS.unpack) . BS.words <$> BS.getLine
  -- ai,bi,ci <= 10^10, length as == x, length bs == y, length cs == z
  let xs = [ sortDown [ as U.! i + bs U.! j + cs U.! k
                      | i <- [0 .. min t (x-1)]
                      , j <- [max 0 (t - z - i + 1) .. min (t-i) (y-1)]
                      , let !k = t-i-j
                      -- i + j + k = t
                      ]
           | t <- [0 .. x + y + z - 3]
           ]
  let result = take k $ mergeLists xs
  mapM_ print result
