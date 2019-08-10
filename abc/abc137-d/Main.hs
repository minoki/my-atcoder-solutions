-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.List (unfoldr)
import Control.Monad
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Char8 as BS
import qualified Data.IntMap.Strict as IntMap

main = do
  [n,m] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  xs <- U.replicateM n $ do
    [a,b] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    return (a,b)
  let ys = V.create $ do
        vec <- VM.replicate (m+1) IntMap.empty
        U.forM_ xs $ \(a,b) -> do
          when (a <= m) $ do
            VM.modify vec (IntMap.insertWith (+) b 1) a
        return vec
  let loop !acc w !i
        | i > m = acc
        | otherwise = let w' = (IntMap.unionWith (+) w (ys V.! i))
                      in if IntMap.null w'
                         then loop acc w' (i+1)
                         else let (b,_) = IntMap.findMax w'
                                  w'' = IntMap.updateMax (\x -> if x == 1 then Nothing else Just (x-1)) w'
                              in loop (acc + b) w'' (i+1)
  print $ loop 0 IntMap.empty 0
