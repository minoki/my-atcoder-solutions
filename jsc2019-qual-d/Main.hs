-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.List
import Control.Monad
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Builder as BSB
import System.IO
import Data.Monoid
import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad.ST

words_BSB :: [BSB.Builder] -> BSB.Builder
words_BSB xs = mconcat $ intersperse (BSB.char7 ' ') xs

lines_BSB :: [BSB.Builder] -> BSB.Builder
lines_BSB xs = mconcat $ map (<> BSB.char7 '\n') xs

solveRec :: STUArray s (Int, Int) Int -> Int -> Int -> U.Vector Int -> ST s Int
solveRec table !s 2 xs = do let [i,j] = U.toList xs
                            writeArray table (i,j) s
                            return (s+1)
solveRec table !s 3 xs = do let [i,j,k] = U.toList xs
                            writeArray table (i,j) s
                            writeArray table (i,k) s
                            writeArray table (j,k) (s+1)
                            return (s+2)
solveRec table !s n xs = do forM_ [1,3..n-1] $ \d -> do
                              forM_ [0..n-1-d] $ \i -> do
                                writeArray table (xs U.! i,xs U.! (i+d)) s
                            let m = n `quot` 2
                            solveRec table (s+1) (n - m) $ U.map (xs U.!) (U.fromList [0,2..n-1])
                            solveRec table (s+1) m $ U.map (xs U.!) (U.fromList [1,3..n-1])

main = do
  n <- readLn
  let result = runSTUArray $ do
        table <- newArray ((1,1),(n,n)) 0
        solveRec table 1 n $ U.enumFromN 1 n
        return table
  BSB.hPutBuilder stdout $ lines_BSB [words_BSB $ [ BSB.intDec (result ! (i,j)) | j <- [i+1..n]] | i <- [1..n-1]]
