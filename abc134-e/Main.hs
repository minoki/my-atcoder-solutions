-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.List (unfoldr)
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Char8 as BS
import Data.Monoid
import qualified Data.IntMap.Strict as IntMap

main = do
  n <- readLn
  xs <- U.replicateM n $ do
    Just (x, _) <- BS.readInt <$> BS.getLine
    return x
  let s = U.foldl (\ !s !y ->
                     case IntMap.lookupLT y s of
                       Nothing -> IntMap.insertWith (+) y 1 s
                       Just (x,k) -> if k == 1 then IntMap.insertWith (+) y 1 $ IntMap.delete x s else IntMap.insertWith (+) y 1 $ IntMap.insert x (k-1) s
                  ) IntMap.empty xs
  print $ getSum $ IntMap.foldMapWithKey (\_ -> Sum) s
