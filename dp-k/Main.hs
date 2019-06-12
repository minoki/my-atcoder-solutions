-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.List (unfoldr)
-- import Control.Monad (forM_)
import qualified Data.Vector.Unboxed as U
-- import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS

main = do
  [n,k] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  -- 1 <= n <= 300, 1 <= k <= 10^5
  xs <- U.unfoldrN n (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  -- 1 <= a1 < a2 < ... < an <= k
  {-
  let vec = U.create $ do
        vec <- UM.new (k + 1)
        UM.write vec 0 False
        forM_ [1..k] $ \i -> do
          result <- not . U.and <$> U.mapM (\x -> UM.read vec (i - x)) (U.takeWhile (<= i) xs)
          UM.write vec i result
        return vec
  -}
  let vec = U.constructN (k + 1) $
        \vec -> let !i = U.length vec
                in not $ U.and $ U.map (\x -> vec U.! (i - x)) $ U.takeWhile (<= i) xs
  -- vec ! i: 石が i 個ある状態で、双方が最善を尽くした時に現在の手番の人が勝つなら True
  putStrLn (if U.last vec then "First" else "Second")
