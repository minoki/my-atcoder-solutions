-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.Int (Int64)
import Data.List (unfoldr)
import Control.Monad (forM_, foldM)
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Char8 as BS
import Data.Array.Unboxed
import Data.Array.ST

main = do
  n <- readLn
  xs <- U.map fromIntegral . U.unfoldrN n (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let ys :: U.Vector Int64
      ys = U.scanl (+) 0 xs
  let arr :: UArray (Int,Int) Int64
      arr = runSTUArray $ do
        arr <- newArray ((0,0),(n,n)) 0
        -- arr ! (i,j) : 半開区間 [i,j) にいるやつを全部合体させるために必要な最小のコスト
        -- 1個のスライムから1個のスライムを得るのに合体は必要ないので arr ! (i,i+1) == 0
        forM_ [2..n] $ \d -> do
          forM_ [0..n-d] $ \i -> do
            let !j = i + d -- 0 <= i < j <= d
            -- c1: これまでの合体で払うコストの最小値
            -- c1 <- minimum <$> sequence [(+) <$> readArray arr (i,k) <*> readArray arr (k,j) | k <- [i+1..j-1]]
            c1 <- foldM (\x a -> min x <$> a) maxBound [(+) <$> readArray arr (i,k) <*> readArray arr (k,j) | k <- [i+1..j-1]]
            let c2 = ys U.! j - ys U.! i -- 今回の合体の際に払うコスト
            writeArray arr (i,j) $! c1 + c2
        return arr
  print $ arr ! (0,n)

-- > minimum <$> sequence [...]
-- よりも
-- > foldM (\x a -> min x <$> a) maxBound
-- の方が速そう（前者はfusionが効かない？）
