import Data.Int
import Control.Monad
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.ByteString.Char8 as BS

main = do
  [n,maxW] <- map (read . BS.unpack) . BS.words <$> BS.getLine
  -- 1 <= n <= 100
  -- 1 <= w <= 10^5
  items <- V.replicateM n $ do
    [wi,vi] <- map (read . BS.unpack) . BS.words <$> BS.getLine
    -- wi <= w, vi <= 10^9
    return (wi :: Int, fromIntegral vi :: Int64)
  let result = V.foldl' (\vec (wi,vi) -> V.generate (maxW+1) (\u -> if u - wi >= 0 then max (vec V.! u) (vec V.! (u - wi) + vi) else vec V.! u)) (V.replicate (maxW + 1) 0) items
  print (V.maximum result :: Int64)
