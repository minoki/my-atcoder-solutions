-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.Int (Int64)
import Data.List (unfoldr)
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Char8 as BS
import qualified Data.IntSet as IntSet
import Control.Exception (assert)
import qualified Test.QuickCheck as QC
import GHC.Stack (HasCallStack)

(!) :: (HasCallStack, U.Unbox a) => U.Vector a -> Int -> a
(!) = (U.!)
{-
(!) vec i | i < 0 = error $ "negative index: " ++ show i
          | i >= U.length vec = error $ "out of bounds " ++ show (i,U.length vec)
          | otherwise = vec U.! i
-}

cycles :: Int -> U.Vector Int -> [[Int]]
cycles !n perm = loop (IntSet.fromDistinctAscList [0..n-1])
  where
    loop s | IntSet.null s = []
           | otherwise = let (m,s') = IntSet.deleteFindMin s
                             (cyc,s'') = oneCycle m m [m] s'
                         in cyc : loop s''
    oneCycle m0 m xs s = let m' = perm ! m
                         in if m' == m0 then
                              (xs, s)
                            else
                              oneCycle m0 m' (m':xs) (IntSet.delete m' s)

solve :: Int -> Int -> U.Vector Int -> U.Vector Int64 -> Int64
solve !n !k perm c = maximum $ map solveOneCycle $ cycles n perm
  where
    solveOneCycle cyc =
      let scores = U.fromList $ map (c !) cyc
          cycle_len = U.length scores
          t = U.sum scores
          scores' = U.init $ scores <> scores
          ss = U.scanl' (+) 0 scores'
          !_ = assert (U.length ss == 2 * cycle_len)
      in if t <= 0 || k <= cycle_len then
           maximum [ ss ! j - ss ! i
                   | i <- [0 .. cycle_len - 1]
                   , j <- [i + 1 .. min (2 * cycle_len - 1) (i + k)]
                   ]
         else
           let (q,r) = k `quotRem` cycle_len
               -- q >= 1
           in t * fromIntegral q + if r == 0 then
                                     max 0 $ maximum [ ss ! j - ss ! i
                                                     | i <- [0 .. cycle_len - 1]
                                                     , j <- [i + 1 .. min (2 * cycle_len - 1) (i + cycle_len)]
                                                     ] - t
                                   else
                                     maximum [ ss ! j - ss ! i
                                             | i <- [0 .. cycle_len - 1]
                                             , j <- [i + 1 .. min (2 * cycle_len - 1) (i + r)]
                                             ]

main = do
  [n,k] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  perm <- U.map (subtract 1) <$> U.unfoldrN n (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  c <- U.map fromIntegral <$> U.unfoldrN n (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  print $ solve n k perm c

prop :: QC.Property
prop = let gen = do n <- QC.choose (2, 100)
                    -- n <- QC.choose (2, 5000)
                    k <- QC.choose (1, 10^9)
                    perm <- QC.shuffle [0..n-1]
                    c <- QC.vectorOf n (QC.choose (-10^9, 10^9))
                    return (n, k, U.fromList perm, U.fromList c)
       in QC.forAll gen (\(n,k,perm,c) -> solve n k perm c `seq` ())
