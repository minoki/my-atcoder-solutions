import Control.Monad
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.ByteString.Char8 as BS

main = do
  n <- readLn
  happiness <- V.replicateM n $ do
    [a,b,c] <- map (read . BS.unpack) . BS.words <$> BS.getLine
    -- a,b,c <= 10^4
    return (a,b,c :: Int)
  let (a,b,c) = V.last $ V.scanl1' (\(a,b,c) (a',b',c') -> (max b c + a', max a c + b', max a b + c')) happiness
  print (max a (max b c))
