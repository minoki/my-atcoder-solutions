-- https://github.com/minoki/my-atcoder-solutions
import Data.Char (isSpace)
import Data.List
import Data.Monoid
import Control.Monad
import qualified Data.ByteString.Char8 as BS

main = do
  n <- readLn
  tasks <- fmap (sortBy (\(a,b) (a',b') -> compare b b' <> compare a a')) $ replicateM n $ do
    [a,b] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    return (a,b)
  let xs = tail $ scanl' (+) 0 $ map fst tasks
  putStrLn $ if and $ zipWith (<=) xs (map snd tasks)
             then "Yes"
             else "No"
