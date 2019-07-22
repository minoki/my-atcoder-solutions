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

mergeTwo :: [(Int64, Int)] -> [(Int64, Int)] -> [(Int64, Int)]
mergeTwo xs@(x@(xv,xn):xss) ys@(y@(yv,yn):yss) = case compare xv yv of
  LT -> y : mergeTwo xs yss
  EQ -> let !zn = xn + yn in (xv, zn) : mergeTwo xss yss
  GT -> x : mergeTwo xss ys
mergeTwo [] ys = ys
mergeTwo xs [] = xs

-- replicate' n x ys = replicate n x ++ ys
replicate' :: Int -> a -> [a] -> [a]
replicate' 0 x ys = ys
replicate' n x ys = x : replicate' (n - 1) x ys

undup :: [(Int64, Int)] -> [Int64]
undup ((xv,xn):xs) = replicate' xn xv $ undup xs
undup [] = []

mergeLists :: [[(Int64, Int)]] -> [(Int64, Int)]
mergeLists [] = []
mergeLists (xs:sss) = doMerge' xs sss
  where
    doMerge' xs (ys:sss) = doMerge xs ys sss
    doMerge' xs [] = xs
    doMerge xs (y:yss) sss = innerLoop xs y yss sss
      where innerLoop xs@(x@(xv,xn):xss) !y@(!yv,!yn) yss sss = case compare xv yv of
              LT -> y : doMerge' (mergeTwo xs yss) sss
              EQ -> let !zn = xn + yn in (xv, zn) : doMerge' (mergeTwo xss yss) sss
              GT -> x : innerLoop xss y yss sss
            innerLoop [] !y yss sss = y : doMerge' yss sss
    doMerge [] ys [] = ys
    doMerge [] ys (zs:sss) = doMerge ys zs sss
    doMerge xs [] (ys:sss) = doMerge xs ys sss
    doMerge xs [] [] = xs

mergeDup :: [Int64] -> [(Int64, Int)]
mergeDup (x:xs) = loop 1 x xs
  where loop !i !x ys@(y:yss) | x == y = loop (i+1) x yss
                              | otherwise = (x,i) : mergeDup ys
        loop !i !x [] = [(x,i)]
mergeDup [] = []

mergeDup2 :: [(Int64, Int)] -> [(Int64, Int)]
mergeDup2 ((x,n):xs) = loop n x xs
  where loop !i !x ys@((y,m):yss) | x == y = loop (i+m) x yss
                                  | otherwise = (x,i) : mergeDup2 ys
        loop !i !x [] = [(x,i)]
mergeDup2 [] = []

sumList :: U.Vector (Int64, Int) -> U.Vector (Int64, Int) -> [(Int64, Int)]
sumList as bs = let !n = U.length as
                    !m = U.length bs
                in mergeLists
                   [ mergeDup2 $ sortDown
                     [ (s, nn)
                     | i <- [max 0 (t-m+1) .. min t (n - 1)]
                     , let !j = t - i
                           (a,an) = as U.! i
                           (b,bn) = bs U.! j
                           !s = a + b
                           !nn = an * bn
                     -- i + j = t
                     ]
                   | t <- [0 .. n + m - 2]
                   ]

main = do
  [x,y,z,k] :: [Int] <- map (read . BS.unpack) . BS.words <$> BS.getLine
  -- x,y,z <= 1000
  -- k <= min 3000 (x*y*z)
  as :: U.Vector (Int64, Int) <- U.fromListN x . mergeDup . sortDown . map (read . BS.unpack) . BS.words <$> BS.getLine
  bs :: U.Vector (Int64, Int) <- U.fromListN y . mergeDup . sortDown . map (read . BS.unpack) . BS.words <$> BS.getLine
  cs :: U.Vector (Int64, Int) <- U.fromListN z . mergeDup . sortDown . map (read . BS.unpack) . BS.words <$> BS.getLine
  -- ai,bi,ci <= 10^10, length as == x, length bs == y, length cs == z
  let a_and_b = U.fromListN (min (U.length as * U.length bs) k) $ take k $ sumList as bs
      result = take k $ undup $ sumList a_and_b cs
  mapM_ print result
