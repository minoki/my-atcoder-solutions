import Data.Char
import Data.List
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Unboxed as U

main = do
  [n,m] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  d <- sequence $ replicate m $ do
    _:ss <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    return ss
  ps <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let result = sum $ do
        t <- U.replicateM n [0,1]
        forM_ (zip d ps) $ \(ss,p) -> do
          guard $ (sum (map ((t U.!) . subtract 1) ss) `rem` 2) == p
        return 1
  print result
