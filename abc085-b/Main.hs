-- https://github.com/minoki/my-atcoder-solutions
import Control.Monad (replicateM)
import qualified Data.IntSet as IntSet

main = do
  n <- readLn
  set <- IntSet.fromList <$> replicateM n readLn
  print $ IntSet.size set
