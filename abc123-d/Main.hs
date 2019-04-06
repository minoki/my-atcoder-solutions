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

mergeLists :: [[(Int64, Int)]] -> [Int64]
mergeLists [] = []
mergeLists (xs:sss) = doMerge' xs sss
  where
    doMerge' xs (ys:sss) = doMerge xs ys sss
    doMerge' xs [] = undup xs
    doMerge xs (y@(yv,yn):yss) sss = innerLoop xs yv yn yss sss
      where innerLoop xs@(x@(xv,xn):xss) !yv !yn yss sss = case compare xv yv of
              LT -> replicate' yn yv $ doMerge' (mergeTwo xs yss) sss
              EQ -> replicate' (xn + yn) xv $ doMerge' (mergeTwo xss yss) sss
              GT -> replicate' xn xv $ innerLoop xss yv yn yss sss
            innerLoop [] !yv !yn yss sss = replicate' yn yv $ doMerge' yss sss
    doMerge [] ys [] = undup ys
    doMerge [] ys (zs:sss) = doMerge ys zs sss
    doMerge xs [] (ys:sss) = doMerge xs ys sss
    doMerge xs [] [] = undup xs

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

main = do
  [x,y,z,k] :: [Int] <- map (read . BS.unpack) . BS.words <$> BS.getLine
  -- x,y,z <= 1000
  -- k <= min 3000 (x*y*z)
  as :: U.Vector (Int64, Int) <- U.fromListN x . mergeDup . sortDown . map (read . BS.unpack) . BS.words <$> BS.getLine
  bs :: U.Vector (Int64, Int) <- U.fromListN y . mergeDup . sortDown . map (read . BS.unpack) . BS.words <$> BS.getLine
  cs :: U.Vector (Int64, Int) <- U.fromListN z . mergeDup . sortDown . map (read . BS.unpack) . BS.words <$> BS.getLine
  -- ai,bi,ci <= 10^10, length as == x, length bs == y, length cs == z
  let !x' = U.length as
      !y' = U.length bs
      !z' = U.length cs
  let xs = [ mergeDup2 $ sortDown
             [ (s, nn)
             | i <- [0 .. min t (x' - 1)]
             , j <- [max 0 (t - z' - i + 1) .. min (t-i) (y'-1)]
             , let !k = t - i - j
                   (a,an) = as U.! i
                   (b,bn) = bs U.! j
                   (c,cn) = cs U.! k
                   !s = a + b + c
                   !nn = an * bn * cn
             -- i + j + k = t
             ]
           | t <- [0 .. x' + y' + z' - 3]
           ]
  let result = take k $ mergeLists xs
  mapM_ print result
