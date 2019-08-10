-- https://github.com/minoki/my-atcoder-solutions
import Data.Char (isSpace)
import Data.Int (Int64)
import Data.List (unfoldr, group, sort)
import Control.Monad
import qualified Data.ByteString.Char8 as BS

main = do
  n <- readLn
  ss <- replicateM n $ do
    BS.sort <$> BS.getLine
  let xs = map (fromIntegral . length) $ group $ sort ss
  print (sum [x*(x-1) `quot` 2 | x <- xs] :: Int64)
