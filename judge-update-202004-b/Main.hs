-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Algorithms.Merge as A

sortVector :: (G.Vector v a, Ord a) => v a -> v a
sortVector v = G.create do
  v' <- G.thaw v
  A.sort v'
  return v'
{-# INLINE sortVector #-}

main = do
  n <- readLn @Int
  balls <- U.replicateM n do
    [s1,s2] <- BS.words <$> BS.getLine
    let x = read @Int $ BS.unpack s1
        [c] = BS.unpack s2
    return (if c == 'R' then 0 else 1, x)
  let sorted :: U.Vector (Int, Int)
      sorted = sortVector balls
  U.forM_ sorted \(_, x) -> print x
