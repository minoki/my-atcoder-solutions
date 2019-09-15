-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Control.Monad
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS
{-
import Data.Array.Unboxed
import Data.Array.ST
-}

{-

count :: Int -> Int -> BS.ByteString -> Int
count !i !j !s | j >= BS.length s = 0
               | s `BS.index` i == s `BS.index` j = 1 + count (i+1) (j+1) s
               | otherwise = 0
-}

solve :: U.Vector Char -> Int -> Int
solve !s !d = min d $ U.maximum $ U.scanr' (\(c1,c2) !x -> if c1 == c2 then x + 1 else 0) 0 $ U.zip s (U.drop d s)

main = do
  n <- readLn :: IO Int
  s <- BS.getLine
  let s' = U.generate (BS.length s) $ BS.index s
  print $ maximum [solve s' d | d <- [1..n `quot` 2]]
  {-
  let u :: UArray (Int,Int) Int
      u = runSTUArray $ do
        arr <- newArray ((0,0),(n,n)) 0
        forM_ [n-1,n-2..0] $ \i -> do
          let !x = s `BS.index` i
          forM_ [n-1,n-2..i+1] $ \j -> do
            when (x == s `BS.index` j) $ do
              v <- readArray arr (i+1,j+1)
              writeArray arr (i,j) $! min (1 + v) (j - i)
        return arr
  print $ maximum $ elems u
  -}
