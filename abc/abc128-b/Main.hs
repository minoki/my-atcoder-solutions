import Control.Monad
import Data.Ord
import Data.List
import qualified Data.ByteString.Char8 as BS

main = do
  n <- readLn
  r <- forM [1..n] $ \i -> do
    [s,ps] <- BS.words <$> BS.getLine
    return (s, Down (read (BS.unpack ps) :: Int), i :: Int)
  let ss = sort r
  forM_ ss $ \(_,_,i) -> print i
